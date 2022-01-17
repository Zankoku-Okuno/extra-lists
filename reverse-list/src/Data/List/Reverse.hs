{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


-- | Snoc-lists: lists where prepending is linear-time, but _appending_ is constant-time.
-- Useful for describing zippers and functional queues/buffers more naturally and safely.
--
-- We call it an `RList` because this is really just a vanilla list, but where
-- the semantics are that the last-added thing (internally cons'ed) is
-- understood to be at the "end" of the list.
--
-- WARNING: the `Foldable` instance provides a `Foldable.toList`; this simply unwraps the `RList` rather than reversing it.
-- If you need to convert from revered semantics to forward semantics, use this module's `toList`.
module Data.List.Reverse
  ( RList
  -- * Introduction and Elimination
  , nil
  , snoc
  , singleton
  , unsnoc
  -- ** Patterns
  , pattern Nil
  , pattern Snoc
  -- * Queries
  , null
  -- * Traversal
  , catMaybes
  -- * Conversion
  , toList
  , fromList
  , reverseIn
  , reverseOut
  , toArrayN
  , toSet
  ) where

import Prelude hiding (null,reverse)

import Control.DeepSeq (NFData)
import Data.Primitive.Contiguous (Contiguous, Element, SmallArray)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Set as Set
import qualified Prelude


newtype RList a = RList { unRList :: [a] }
  deriving stock (Show,Generic)
  deriving newtype (Functor,Foldable)
instance (NFData a) => NFData (RList a)

nil :: RList a
{-# INLINABLE nil #-}
nil = RList []

snoc :: RList a -> a -> RList a
{-# INLINABLE snoc #-}
snoc (RList xs) x = RList (x:xs)

unsnoc :: RList a -> Maybe (RList a, a)
{-# INLINABLE unsnoc #-}
unsnoc (RList []) = Nothing
unsnoc (RList (x:xs)) = Just (RList xs, x)

{-# COMPLETE Nil, Snoc #-}

pattern Nil :: RList a
pattern Nil = RList []

pattern Snoc :: RList a -> a -> RList a
pattern Snoc xs x <- (unsnoc -> Just (xs, x))
  where Snoc = snoc

singleton :: a -> RList a
{-# INLINE singleton #-}
singleton = RList . (:[])

null :: RList a -> Bool
{-# INLINE null #-}
null (RList xs) = List.null xs

catMaybes :: RList (Maybe a) -> RList a
{-# INLINE catMaybes #-}
catMaybes = RList . Maybe.catMaybes . unRList

-- | @O(n)@ Convert to a plain list, maintaining order.
--
-- This is here so that you can escape back out to normal cons-list land once
-- you're done building your list.
--
-- See 'reverseOut' for when order doesn't matter.
toList :: RList a -> [a]
{-# INLINE toList #-}
toList (RList xs) = Prelude.reverse xs

-- | @O(n)@ Convert from a plain list, maintaining order.
--
-- This is added for completion's sake, as I'm not sure you'll often need this adapter.
--
-- See `toList` for the inverse, or `reversIn` for when order doesn't matter.
fromList :: [a] -> RList a
{-# INLINE fromList #-}
fromList = RList . Prelude.reverse

-- | @O(0)@ Reverse an `RList`, returning a plain cons list.
--
-- This is here so that when the output list is fed to an order-agnostic
-- function, you don't have to pay the cost of reversing the representation.
--
-- See 'toList' for when order matters.
reverseOut :: RList a -> [a]
{-# INLINE reverseOut #-}
reverseOut = unRList


-- | @O(0)@ Reverse a plain cons list, rerutning an `RList`.
--
-- See `reverseOut` for the inverse, and why you might use these.
reverseIn :: [a] -> RList a
{-# INLINE reverseIn #-}
reverseIn = RList

-- | Write the contents of the `RList` into an array, assuming you know the length of the array.
-- This is useful in the common case of buffering an unknown-length stream before allocating contiguous space for the elements.
--
-- If you sepcify to small a langth, the initial part of the array will be uninitialized.
-- If you specify to large a length, the initial part of the list will not be written.
--
-- If you are unaware of the size of the list, `Arr.fromList . fromList` will do the trick, but will obviously be slower.
toArrayN :: (Contiguous arr, Element arr a) => Int -> RList a -> arr a
{-# INLINE toArrayN #-} -- use inline instead of inlinable, because inlinable with Contiguous is busted
{-# SPECIALIZE toArrayN :: Int -> RList a -> SmallArray a #-}
toArrayN n (RList xs0) = Arr.create $ do
  mut <- Arr.new n
  loop mut (n - 1) xs0
  pure mut
  where
  loop _ (-1) _ = pure ()
  loop _ _ [] = pure ()
  loop arr i (x:xs) = do
    Arr.write arr i x
    loop arr (i - 1) xs

toSet :: (Ord a) => RList a -> Set a
{-# INLINABLE toSet #-}
toSet (RList xs) = Set.fromList xs
