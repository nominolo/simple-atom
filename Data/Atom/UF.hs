{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-| 
Module      : Data.Atom.UF
Copyright   : (c) Thomas Schilling 2010
License     : BSD-style

Maintainer  : nominolo@gmail.com
Stability   : experimental
Portability : portable

Symbols without a central symbol table.

Symbols provide the following efficient operations:

 - /O(1)/ equality comparison (in practise)

 - /O(1)/ ordering comparison (in practise)

 - /O(n)/ creation where /N/ is the size of the symbol descriptor.

Many implementations often have the additional property that each
symbol descriptor only exists once in memory.  This implementation
slightly relaxes this property:

 - A symbol descriptor is guaranteed to exists only once in memory
   if it has been created using the same symbol table.
   Furthermore, if two symbols created from different symbol tables
   are compared and their descriptors turn out to be equal, the
   symbols will share the descriptor after the comparison.

This allows the following additional properties not present in
conventional implementations:

 - No space leak.  The symbol table can be discarded at any time.

 - Symbols created using different symbol tables can be compared
   reliably.

 - No global lock.  (TODO: Well we might need one in the case of
   hash-collisions, but a lock-free implementation might be
   possible.)

Inspired by Richard O'Keefe's message to Erlang's eeps mailing list
<http://www.erlang.org/cgi-bin/ezmlm-cgi/5/057>, which in turn was
inspired by the Logix implementation of Flat Concurrent Prolog.

-}
module Data.Atom.UF (
  -- * Symbols
  --
  Symbol, intern, internInto, SymTab(..), symbolHash
  -- * Implementation
  --
  -- $impl
  --
)
where

import Data.Word ( Word64 )
import Data.IORef
import System.IO.Unsafe
import Control.Monad ( unless )
import Control.DeepSeq

#ifndef NDEBUG
-- For testing:
import Control.Monad ( liftM2 )
import System.Mem.Weak
import System.Mem
import Data.Digest.Murmur64
import Data.Maybe ( isJust )
#endif

-- -------------------------------------------------------------------
-- Public API:

-- | A symbol.
--
-- Note that the ordering on @a@ is /not/ preserved on @Symbol a@.
-- Symbols are ordered by their hashes, and only if the hashes are
-- equal will the ordering on @a@ be used.  We have:
--
-- @
--  x == y ==> intern x == intern y
--
--  let sx = intern x
--      sy = intern y
--  in
--    (sx < sy) == ((symbolHash sy < symbolHash sx) ||
--                  symbolHash sy == symbolHash sx && x < y)
-- @
data Symbol a =
  Symbol {-# UNPACK #-} !Word64 -- hash
         {-# UNPACK #-} !(IORef (SymbolInfo a))

-- | Returns the hash of the symbol.
symbolHash :: Symbol a -> Word64
symbolHash (Symbol h _) = h

instance Ord a => Eq (Symbol a) where x == y = cmpSymbol x y == EQ
instance Ord a => Ord (Symbol a) where compare = cmpSymbol
instance Show a => Show (Symbol a) where
  show = show . symInfo

-- | Create a new local symbol.  For best performance use
-- 'internInto' together with a symbol table / map.
intern :: (a -> Word64) -> a -> Symbol a

class SymTab s where
  lookupSymbol :: s a -> a -> Maybe (Symbol a)
  insertSymbol :: a -> (Symbol a) -> s a -> s a

-- | Insert a symbol into an existing table.
internInto :: (SymTab s) => (a -> Word64) -> s a -> a -> (s a, Symbol a)

-- -------------------------------------------------------------------
-- Internals

newtype SymbolInfo a = SymInfo (IORef (Link a))

type Link a = Either a (SymbolInfo a)

internInto hash_fn st str =
  case lookupSymbol st str of
    Just sym -> (st, sym)
    _        -> let sym = intern hash_fn str in
                (insertSymbol str sym st, sym)

intern hash_fn s = unsafePerformIO $ do
  info <- newIORef (Left s)
  info' <- newIORef (SymInfo info)
  return (Symbol (hash_fn s) info')

cmpSymbol :: Ord a => Symbol a -> Symbol a -> Ordering
cmpSymbol (Symbol h1 i1) (Symbol h2 i2)
  | i1 == i2 = EQ
  | otherwise =
     case h1 `compare` h2 of
       EQ -> uncommon_case   -- not identical, but same hash
       ans -> ans
 where
   {-# NOINLINE uncommon_case #-}
   uncommon_case = unsafePerformIO $ do
     -- get representative element (performs path shortening)
     (rep1@(SymInfo rr1), s1) <- repr' i1 
     (rep2@(SymInfo rr2), s2) <- repr' i2
     if rep1 === rep2 then
       return EQ
      else
       case s1 `compare` s2 of
         EQ -> do -- they should be equal!
           writeIORef rr2 (Right rep1)
           writeIORef i2 rep1
           return EQ
         ans -> return ans


-- We abuse the fact that IORefs give us an identity (i.e., observable
-- sharing) and that we need the IORef anyway.
sameSym :: SymbolInfo a -> SymbolInfo a -> Bool
sameSym (SymInfo r1) (SymInfo r2) = r1 == r2

(===) = sameSym

symInfo :: Symbol a -> a
symInfo (Symbol _ r) = unsafePerformIO $ do
  fmap snd (repr' r)

repr' :: IORef (SymbolInfo a) -> IO (SymbolInfo a, a)
repr' r = do
  info <- readIORef r
  (root_info, str) <- go info
  unless (root_info === info) $
    writeIORef r root_info
  return (root_info, str)
 where
   go si@(SymInfo ir) = do
     i <- readIORef ir
     case i of
       Left str -> return (si, str)
       Right si' -> do
         (root_info, str) <- go si'
         unless (si' === root_info) $
           writeIORef ir (Right root_info)   -- is Left possible here?
         return (root_info, str)

----------------------------------------------------------
-- Tests
#ifndef NDEBUG
-- requires import Data.Digest.Murmur32

test1 = do
  let h = asWord64 . hash64
      s1@(Symbol _ r1) = intern h "foo"
      s2@(Symbol _ r2) = intern h "foo"
  print $ r1 == r2   -- should be False
                     
  -- create a weak reference to the second symbol, so we can observe
  -- when it is garbage collected
  w <- mk_weak =<< readIORef r2

  print $ s1 == s2   -- should print True
  print =<< liftM2 sameSym (readIORef r1) (readIORef r2) -- should print True
  putStrLn "GCing"
  performGC          -- this should print goodbye, representing the
                     -- fact that the second symbol has been garbage
                     -- collected.
  print . isJust =<< deRefWeak w  -- should print False (object has been collected)
 where
   mk_weak o = mkWeakPtr o (Just (putStrLn "goodbye"))

#endif

{- $doc1

test 1

-}

{- $impl

Each symbol is represented a mutable pointer to the symbol info and a
hash.  The symbol info might itself be a pointer to another (equal)
symbol info.

When creating a new symbol (without looking it up in a symbol table),
we compute its hash and create a new symbol info.

>     +----+---+     +-------+
> A:  | 42 | *-----> | "foo" |
>     +----+---+     +-------+

We now know the following:

 1. If two symbols have the same reference, they are equal.  (The 'Eq'
    instance on 'IORef's implements observable sharing.)

 2. If two symbols have a different hash, they are different.

If neither of the above is true we either have a hash collision or the
two objects are equal but were created using different symbol tables.


Let's consider the latter case:

>      +----+---+     +-------+
>  A:  | 42 | *-----> | "foo" |
>      +----+---+     +-------+
>
>      +----+---+     +-------+
>  B:  | 42 | *-----> | "foo" |
>      +----+---+     +-------+


We follow the symbol pointers and realise that the symbol descriptors
are equal.  We thus decide for one of them to be the canonical symbol
descriptor and update the pointers:


>      +----+---+     +-------+
>  A:  | 42 | *-----> | "foo" |
>      +----+---+ .-> +-------+
>                 |       ^
>      +----+---+ |   +---|---+
>  B:  | 42 | *---'   |   *   |
>      +----+---+     +-------+

We change the other symbol descriptor to be a pointer to the canonical
descriptor, because there may be other pointers to this symbol
descriptor.  Otherwise, the old symbol descriptor becomes garbage.  We
now have only one @\"foo\"@ object left.

We can add a third rule for equality:

 - If, following all pointers, two symbol descriptors are the same,
   then the two symbols are equal.

If this is not the case (e.g., in the case of a hash collision) we
call the 'compare' function of the symbol descriptor.  A good hash
function is therefore important since in the case of a hash collision
we will always have to call the 'compare' function of the symbol
descriptor.


** Hash Function

Assuming a good hash function (i.e., the hash is indistinguishable
from a randomly generated number) we can use the birthday paradox to
calculate the probability of a hash collision:

@
collision_prob :: Integer -> Integer -> Double
collision_prob key_bits items =
    1 - exp (fromIntegral (-items * (items - 1)) / fromIntegral (2 * key_space))
  where
    key_space = 2 ^ key_bits :: Integer
@

E.g., @collision_prob 32 50000 == 0.2525...@ means that with
32 bit hashes and 50000 symbols, there is a 25 percent chance of a
hash collision.


** Path Shortening

If symbols from several symbol tables are joined repeatedly, its
symbol infos may develop into long chains.  For this reason we update
all pointers while following them.

That is, given we have the following state:

> X+-sym+---+     A+-nfo-+    B+-------+
>  | 42 | *------->|  *------> | "foo" |
>  +----+---'      +-----'     '-------'
>
> Y+-sym+---+     C+-nfo-+    D+-------+
>  | 42 | *------->|  *------> | "foo" |
>  +----+---'      +-----'     '-------'

after @x \`compare\` y@ we have.


> X+-sym+---+      B+-------+    A+-nfo-+    
>  | 42 | *-------> | "foo" |<-------*  |
>  `----+---'  +--> '-------'     `-----'
>              |          ^ ^------+
> Y+-sym+---+  |  C+-nfo-+|   D+---|---+
>  | 42 | *----'   |  *---'    |   *   |
>  `----+---'      `-----'     '-------'

These references can be updated concurrently and without a lock since
their information content does not change.  That is, the state

> X.-sym+---.     A.-nfo-.    B.-------.
>  | 42 | *------->|  *------> | "foo" |
>  `----+---'      `-----'     '-------'

Is semantically equivalent to the state:

> X.-sym+---.     A.-nfo-.    B.-------.
>  | 42 | *----.   |  *------> | "foo" |
>  +----+---+  |   +-----+ .-> +-------+
>              '-----------'



* TODO

 - verify thread-safety

 - make sure the pointer update code is correct and has no bad
   cases

 - implement IntMap variant\/wrapper that respects that two
   different objects may have the same hash (however unlikely).


-}