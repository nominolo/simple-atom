{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | 
-- Module      : Data.Atom.UF
-- Copyright   : (c) Thomas Schilling 2010
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Symbols without a central symbol table.
--
-- Symbols provide the following efficient operations:
--
--  - /O(1)/ equality comparison (in practise)
--  - /O(1)/ ordering comparison (in practise)
--  - /O(n)/ creation
--
-- This can be implemented by using a global variable mapping strings
-- to symbols and a counter assigning ids to symbols.  However, this
-- has two problems:
--
--  1. It has a space leak.  No symbols can ever be removed from this
--     table.  For example, if we add the symbol @\"foo\"@ the first
--     time it might get assigned id 1, if we then delete it and
--     insert it again it might get assigned id 42.  However, there
--     may still be symbols in memory which got assigned id 1.
--     Instead, symbols should be garbage collected like other data.
--     Using weak pointers has bad effects on performance due to
--     garbage collector overhead.
--
--  2. It is not reliable to compare symbols created using different
--     symbol tables.  They would most likely get assigned different
--     ids.
--
-- This implementation of symbols allows *optional* use of a symbol
-- table.  If a symbol table is used, this implementation will tend to
-- use less memory and its operations will be a little bit faster at
-- the beginning.  For longer runs, it won't make a big difference
-- though, since the representation is self-optimising.
--
-- Inspired by Richard O'Keefe's message to Erlang's eeps mailing list
-- <http://www.erlang.org/cgi-bin/ezmlm-cgi/5/057>, which in turn was
-- inspired by the Logix implementation of Flat Concurrent Prolog.
--
--
-- * Implementation
--
-- Each symbol is represented a pointer to the symbol info, which
-- consists of:
--
--   * a 'String'
--   * a 'Hash'
--   * a null-able parent pointer to an equivalent symbol info
--
-- Creating the same symbol twice will at first be represented as two
-- different entities.
--
-- @
--            .----+-------+-----.
--   A -----> | 42 | "foo" | nil |
--            '----+-------+-----'
--   B --.
--       '--> .----+-------+-----.
--   C -----> | 42 | "foo" | nil |
--            '----+-------+-----'
-- @
--
-- (Note that @A@, @B@ and @C@ are @IORefs@.)
--
-- When comparing @A@ and @B@ we use the following properties:
--
--  1. If @A@ and @B@ are identical then they must be equal.
--  
--  2. If they point to the same object, they must equal.
--  
--  3. If they have different hashes, they are different.
--
-- Unless there is a hash collision, we can decide equality and
-- ordering for all symbols that have been built with the same hash
-- table.
--
-- If the two objects have no parent, have the same hash, and the same
-- string, we now make one the first the parent of the other and
-- update the pointer of @B@ accordingly.  If there are no references
-- to the second object left it can now be garbage collected.
--
-- If an object already has a parent pointer we follow each object's
-- parents to the roots and compare the roots.  This process might
-- again result in updates to @A@ or @B@ and various parent pointers.
--
-- In the example above, after @A == B@ we have:
--
-- @
--            .----+-------+-----.
--   A -----> | 42 | "foo" | nil |
--       .--> '----+-------+-----'
--   B --'                    ^
--            .----+-------+--|--.
--   C -----> | 42 | "foo" |  *  |
--            '----+-------+-----'
-- @
--
-- After @C == A@ or @C == B@ we have.
--
-- @
--   A -----> .----+-------+-----.
--       .--> | 42 | "foo" | nil |
--   B --'.-> '----+-------+-----'
--        |                   ^
--        |   .----+-------+--|--.
--   C ---'   | 42 | "foo" |  *  |
--            '----+-------+-----'
-- @
--
-- The second object will now be garbage collected.
--
-- In fact, after the first @A == B@, the remaining updates could use
-- some help from the garbage collector.  This could be done by
-- somehow forcibly (and unsafely) replacing the second object by an
-- update frame and then rely on the GC's indirection shortening
-- feature.  This is /very/ unsafe, since some code may rely \"know\"
-- that the object is already evaluated.  E.g., C's pointer could be
-- tagged (c.f. \"Faster Laziness Using Dynamic Pointer Tagging\").
-- It /might/ work if we can match the physical layout of both
-- structures, but it's equally likely that hell freezes over, so I'll
-- leave that as an exercise for more braver hackers.
--
-- * TODO
--
--  - generalise to arbitrary hashable objects.  need not be
--    restricted to 'String'.
--
--  - make thread-safe.  (we only need a lock for the uncommon cases)
--
--  - make sure the pointer update code is correct and has no bad
--    cases
--
--  - implement IntMap variant\/wrapper that respects that two
--    different objects may have the same key (however unlikely).
-- 
module Data.Atom.UF 
  ( Symbol, intern, internInto, SymTab(..) )
where

import Data.Word ( Word32 )
import Data.Char ( ord )
import Data.Bits ( xor )
import Data.IORef
import System.IO.Unsafe
import Control.Monad -- ( unless )
import System.Mem.Weak
import System.Mem
import Data.Maybe

-- -------------------------------------------------------------------
-- Public API:

-- | A symbol.
newtype Symbol = Symbol (IORef SymbolInfo)
instance Eq Symbol where x == y = cmpSymbol x y == EQ
instance Ord Symbol where compare = cmpSymbol
instance Show Symbol where show = showSym

-- | Create a new local symbol.  For best performance use
-- 'internInto' together with a symbol table / map.
intern :: String -> Symbol

class SymTab s where
  lookupSymbol :: s -> String -> Maybe Symbol
  insertSymbol :: String -> Symbol -> s -> s

-- | Insert a symbol into an existing table.
internInto :: SymTab s => s -> String -> (s, Symbol)

-- -------------------------------------------------------------------
-- Internals

data SymbolInfo =
  SymInfo {-# UNPACK #-} !Word32  -- hash
          {-# UNPACK #-} !(IORef Link) -- parent [really unpack]?
          String

type Link = Maybe SymbolInfo


internInto st str =
  case lookupSymbol st str of
    Just sym -> (st, sym)
    _        -> let sym = intern str in
                (insertSymbol str sym st, sym)

showSym :: Symbol -> String
showSym (Symbol r) = unsafePerformIO $ do
  -- dupable/inline is fine, too, since the string never changes
  (SymInfo _ _ str) <- readIORef r
  return str

intern s = unsafePerformIO $ do
  lnk <- newIORef Nothing
  r <- newIORef $ SymInfo (hash s) lnk s
  return (Symbol r)

mkSymbolInfo :: String -> SymbolInfo
mkSymbolInfo s = unsafePerformIO $ do
  lnk <- newIORef Nothing
  return $ SymInfo (hash s) lnk s

cmpSymbol :: Symbol -> Symbol -> Ordering
cmpSymbol (Symbol r1) (Symbol r2)
  | r1 == r2 = EQ
  | otherwise = unsafePerformIO $ do
      -- We only read.  It should be safe to use unsafeInlineIO for
      -- the two reads.
      sym1@(SymInfo h1 l1 s1) <- readIORef r1
      sym2@(SymInfo h2 l2 s2) <- readIORef r2
      case h1 `compare` h2 of
        -- If the hashes are different they cannot be the same symbol
        LT -> return LT
        GT -> return GT
        EQ
         | sameSym sym1 sym2 ->
          -- The two references are not the same, but they point to
          -- the same object.  That's fine, we can't optimise any
          -- further.
           return EQ

        -- END OF COMMON CASE
        -- 
        -- If the symbols have been built using the same symbol table
        -- we will only reach this case if we have a hash collision or
        -- the symbols were built from different symbol tables.
        --
        -- TODO: Extract into NOINLINE function, wrap unsafePerformIO,
        -- and use an MVar-based lock.

         | otherwise -> do
          -- The hashes are the same.  It could be a collision, or the
          -- symbol was created using a different symbol table.
          --
          -- Case 1: The symbols have already be joined, but this
          -- Symbol's IORef still points to the old version.  We can
          -- determine this by following the union/find structure.
          rep1 <- repr sym1
          rep2 <- repr sym2
          let string_cmp = s1 `compare` s2  -- lazy!
          if sameSym rep1 rep2 || string_cmp == EQ then do
             -- They should in fact be the same symbol.  Update the
             -- atoms and the symbol infos if necessary.
             -- TODO: Use MVar / lock.
             unless (sameSym sym1 rep1) $ do
               writeIORef r1 rep1
               writeIORef l1 (Just rep1)  -- path shortening
             unless (sameSym sym2 rep1) $ do
               writeIORef r2 rep1
               writeIORef l2 (Just rep1)
             return EQ
            else do
              -- They are not the same, and they shouldn't
              return string_cmp
{-# NOINLINE cmpSymbol #-}

-- We abuse the fact that IORefs give us an identity (i.e., observable
-- sharing) and that we need the IORef anyway.
sameSym :: SymbolInfo -> SymbolInfo -> Bool
sameSym (SymInfo _ r1 _) (SymInfo _ r2 _) = r1 == r2

repr :: SymbolInfo -> IO SymbolInfo
repr sym@(SymInfo _ r _) = do
  parent <- readIORef r   -- TODO: perform path shortening.
  case parent of
    Nothing -> return sym
    Just sym' -> repr sym'

test1 = do
  let s1@(Symbol r1) = intern "foo"
      s2@(Symbol r2) = intern "foo"
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

-- -------------------------------------------------------------------

-- Fowler / Noll / Vo (FNV) hash.  Original code expected 'unsigned
-- char' input.  Don't know whether it behaves worse for unicode
-- chars.
hash :: String -> Word32
hash str = go magic_start (map ord str)
  where
    magic_start = 2166136261 :: Word32
    go :: Word32 -> [Int] -> Word32
    go !h [] = h
    go !h (c:cs) =
        go ((h * 16777619) `xor` fromIntegral c) cs
