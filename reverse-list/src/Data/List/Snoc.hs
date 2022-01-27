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
-- understood to be at the \"end\" of the list.
--
-- WARNING: the `Foldable` instance provides a `Foldable.toList`; this simply unwraps the `RList` rather than reversing it.
-- If you need to convert from revered semantics to forward semantics, use this module's `toList`.
module Data.List.Snoc
  ( RList
  , Tsil
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
  , init
  , last
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

import Prelude hiding (null,init,last,reverse)

import Control.Applicative(Alternative(..))
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Primitive.Contiguous (Contiguous, Element, SmallArray)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Set as Set
import qualified Prelude

-- | This datatype defines snoc-lists: lists with O(1) append and O(n) prepend.
-- Underneath the hood, it is just a plain list, but understood as containing its elements in reverse order.
newtype RList a = RList { unRList :: [a] }
  deriving stock (Generic)
  deriving newtype (Functor,Applicative)
instance (NFData a) => NFData (RList a)

instance (Show a) => Show (RList a) where
  show = show . toList
instance (Read a) => Read (RList a) where
  readsPrec i = (fmap . first) fromList . readsPrec i

instance Semigroup (RList a) where
  (RList a) <> (RList b) = RList (b <> a)
instance Monoid (RList a) where
  mempty = Nil

instance Alternative RList where
  empty = mempty
  (RList a) <|> (RList b) = RList (b <|> a)

-- | See? It's \"List\" in reverse?
-- I dunno, I just think 'RList' is an inelegant name, and word-initial @/t͜s/@ is one of my favorite phonemes.
type Tsil = RList

{-# COMPLETE Nil, Snoc #-}

-- | An empty 'RList', such as 'nil'.
pattern Nil :: RList a
pattern Nil = RList []

-- | The 'RList' consisting of initial and last elements, such as created by 'snoc'.
pattern Snoc :: RList a -> a -> RList a
pattern Snoc xs x <- (unsnoc -> Just (xs, x))
  where Snoc = snoc

-- | The empty 'RList'.
nil :: RList a
{-# INLINABLE nil #-}
nil = RList []

-- | @O(1)@ Append an element.
--
-- If you are looking for @cons@, you should use a plain list, or a finite sequence/queue type.
snoc :: RList a -> a -> RList a
{-# INLINABLE snoc #-}
snoc (RList xs) x = RList (x:xs)

-- | @O(1)@ Access the last element and initial portion of the list.
-- See also 'last' and 'init' if you only need one component.
--
-- If you are looking for @uncons@, you should use a plain list, or a finite sequence/queue type.
unsnoc :: RList a -> Maybe (RList a, a)
{-# INLINABLE unsnoc #-}
unsnoc (RList []) = Nothing
unsnoc (RList (x:xs)) = Just (RList xs, x)

-- | Create a single-element 'RList'.
singleton :: a -> RList a
{-# INLINE singleton #-}
singleton = RList . (:[])

-- | Test if an 'RList' is empty.
null :: RList a -> Bool
{-# INLINE null #-}
null (RList xs) = List.null xs

-- | @O(1)@ extract the last element of a list, if it exists.
-- See also 'unsnoc' if you also need 'init' at the same time.
last :: RList a -> Maybe a
last Nil = Nothing
last (Snoc _ x) = Just x

-- | @O(1)@ extract the elements of a list other than the last, if they exist.
-- See also 'unsnoc' if you also need 'last' at the same time.
init :: RList a -> Maybe (RList a)
init Nil = Nothing
init (Snoc xs _) = Just xs

-- | Remove all 'Nothing's from an 'RList' of 'Maybe's.
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
-- See `toList` for the inverse, or `reverseIn` for when order doesn't matter.
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

-- | Convert to a set without an intermediate conversion to a cons-list.
toSet :: (Ord a) => RList a -> Set a
{-# INLINABLE toSet #-}
toSet (RList xs) = Set.fromList xs
