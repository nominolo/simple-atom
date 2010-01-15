module Data.Atom.Simple
  ( Symbol, intern )
where

import Data.Char ( ord )
import qualified Data.Map as M
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef ( newIORef, atomicModifyIORef )

-- -------------------------------------------------------------------
-- Public Interface

-- | A 'Symbol'.  This is essentially a 'String', but with different
-- performance characteristics:
--
--  * @O(n)@ creation time (using 'insert')
--
--  * @O(1)@ equality comparison.
--
--  * @O(1)@ comparison (in practice).  The result of 'compare' is
--    independent of evaluation order.
--
-- It is currently implemented as follows.
--
--  * Each symbol contains a unique integer, which allows @O(1)@
--    comparison.
--
--  * Each symbol contains an infinite chain of hashes, these are used
--    for comparison.  In practice, it is very rare that more than the
--    first of those hashes is ever evaluated.  The first hash is
--    cached, so that most comparisons will not need any indirections.
--
--  * The 'String' representation of the symbol.  Use 'show' to return
--    it.  At any time, there will be only one symbol of a given name
--    in memory.
--
data Symbol = MkSymbol {-# UNPACK #-} !Int   -- identity
                       {-# UNPACK #-} !Int   -- 1st hash
                                      [Int]  -- other hashes
                                      String -- name

instance Show Symbol where
  show (MkSymbol i h _ s) = "<" ++ show i ++ "," ++ show h ++ ">" ++ s

instance Eq Symbol where
  MkSymbol i1 _ _ _ == MkSymbol i2 _ _ _ = i1 == i2

instance Ord Symbol where
  MkSymbol i1 c1 cs1 _ `compare` MkSymbol i2 c2 cs2 _
    | i1 == i2   = EQ
    | otherwise  =
       case c1 `compare` c2 of
         EQ -> cs1 `compare` cs2
         ans -> ans

-- | Turn a 'String' into a 'Symbol'.
--
-- Note, however, that this function contains a space leak.  It has
-- internal state (the symbol table) but is referentially transparent.
-- Unfortunately, there is no way to delete items from the symbol
-- table.
--
-- (This function is, of course, thread-safe.)
intern :: String -> Symbol
intern =
  unsafePerformIO $
    do tab <- newIORef (SymTbl 0 M.empty)
       return $ \s ->
         unsafePerformIO $
           atomicModifyIORef tab $ \tbl@(SymTbl n t) ->
             case M.lookup s t of
               Just sym -> (tbl, sym)
                
               Nothing ->
                 let n' = n + 1
                     sym = mkSymbol n s
                 in (SymTbl n' (M.insert s sym t), sym)
{-# NOINLINE intern #-}

-- -------------------------------------------------------------------
-- TODO: Use a trie?  Or a hash table?
data SymTbl = SymTbl !Int !(M.Map String Symbol)

mkSymbol :: Int -> String -> Symbol
mkSymbol sym_identity str = MkSymbol sym_identity h hashes str
  where
    ints = map ord str
    (h:hashes) = [ hash p ints | p <- bigprimes ]

---------------------------------------------------------------------------
-- hash stuff

hash :: Int -> [Int] -> Int
hash p []     = 0
hash p (i:is) = i + p * hash p is

-- Note: While these aren't very efficient it is unlikely that more
-- than the first couple of elements are ever evaluated during the
-- whole run of a program.
primes, bigprimes :: [Int]
primes = 2 : [ n | n <- [3..], all (n !/) (takeWhile (<= sqr n) primes) ]
 where
  a !/ b = a `mod` b /= 0
  sqr    = floor . sqrt . fromIntegral

bigprimes = dropWhile (<= 258) primes

