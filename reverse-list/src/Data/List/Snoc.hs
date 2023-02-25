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
module Data.List.Snoc
  ( Tsil
  , RList
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
--
-- | See? It's \"List\" in reverse?
-- I dunno, I just think 'RList' is an inelegant name, and word-initial @/t͜s/@ is one of my favorite phonemes.
newtype Tsil a = Tsil { unTsil :: [a] }
  deriving stock (Generic,Eq)
  deriving newtype (Functor,Applicative)
instance (NFData a) => NFData (Tsil a)

instance (Show a) => Show (Tsil a) where
  show = show . toList
instance (Read a) => Read (Tsil a) where
  readsPrec i = (fmap . first) fromList . readsPrec i

instance Foldable Tsil where
  {-# INLINABLE foldr #-}
  foldr _ z Nil = z
  foldr f z (xs `Snoc` x) = foldr f (x `f` z) xs

instance Semigroup (Tsil a) where
  (Tsil a) <> (Tsil b) = Tsil (b <> a)
instance Monoid (Tsil a) where
  mempty = Nil

instance Alternative Tsil where
  empty = mempty
  (Tsil a) <|> (Tsil b) = Tsil (b <|> a)

-- | I initially went with this boring name for reverse-lists.
-- However, I genuinely would rather write (and pronounce) 'Tsil'.
{-# DEPRECATED RList "Preferred spelling is `Tsil`" #-}
type RList = Tsil

{-# COMPLETE Nil, Snoc #-}

-- | An empty 'RList', such as 'nil'.
pattern Nil :: Tsil a
pattern Nil = Tsil []

-- | The 'Tsil' consisting of initial and last elements, such as created by 'snoc'.
pattern Snoc :: Tsil a -> a -> Tsil a
pattern Snoc xs x <- (unsnoc -> Just (xs, x))
  where Snoc = snoc

-- | The empty 'Tsil'.
nil :: Tsil a
{-# INLINABLE nil #-}
nil = Tsil []

-- | @O(1)@ Append an element.
--
-- If you are looking for @cons@, you should use a plain list, or a finite sequence/queue type.
snoc :: Tsil a -> a -> Tsil a
{-# INLINABLE snoc #-}
snoc (Tsil xs) x = Tsil (x:xs)

-- | @O(1)@ Access the last element and initial portion of the list.
-- See also 'last' and 'init' if you only need one component.
--
-- If you are looking for @uncons@, you should use a plain list, or a finite sequence/queue type.
unsnoc :: Tsil a -> Maybe (Tsil a, a)
{-# INLINABLE unsnoc #-}
unsnoc (Tsil []) = Nothing
unsnoc (Tsil (x:xs)) = Just (Tsil xs, x)

-- | Create a single-element 'Tsil'.
singleton :: a -> Tsil a
{-# INLINE singleton #-}
singleton = Tsil . (:[])

-- | Test if an 'Tsil' is empty.
null :: Tsil a -> Bool
{-# INLINE null #-}
null (Tsil xs) = List.null xs

-- | @O(1)@ extract the last element of a list, if it exists.
-- See also 'unsnoc' if you also need 'init' at the same time.
last :: Tsil a -> Maybe a
{-# INLINABLE last #-}
last Nil = Nothing
last (Snoc _ x) = Just x

-- | @O(1)@ extract the elements of a list other than the last, if they exist.
-- See also 'unsnoc' if you also need 'last' at the same time.
init :: Tsil a -> Maybe (Tsil a)
{-# INLINABLE init #-}
init Nil = Nothing
init (Snoc xs _) = Just xs

-- | Remove all 'Nothing's from an 'Tsil' of 'Maybe's.
catMaybes :: Tsil (Maybe a) -> Tsil a
{-# INLINE catMaybes #-}
catMaybes = Tsil . Maybe.catMaybes . unTsil

-- | @O(n)@ Convert to a plain list, maintaining order.
--
-- This is here so that you can escape back out to normal cons-list land once
-- you're done building your list.
--
-- See 'reverseOut' for when order doesn't matter.
toList :: Tsil a -> [a]
{-# INLINE toList #-}
toList (Tsil xs) = Prelude.reverse xs

-- | @O(n)@ Convert from a plain list, maintaining order.
--
-- This is added for completion's sake, as I'm not sure you'll often need this adapter.
--
-- See `toList` for the inverse, or `reverseIn` for when order doesn't matter.
fromList :: [a] -> Tsil a
{-# INLINE fromList #-}
fromList = Tsil . Prelude.reverse

-- | @O(0)@ Reverse an `Tsil`, returning a plain cons list.
--
-- This is here so that when the output list is fed to an order-agnostic
-- function, you don't have to pay the cost of reversing the representation.
--
-- See 'toList' for when order matters.
reverseOut :: Tsil a -> [a]
{-# INLINE reverseOut #-}
reverseOut = unTsil


-- | @O(0)@ Reverse a plain cons list, rerutning an `Tsil`.
--
-- See `reverseOut` for the inverse, and why you might use these.
reverseIn :: [a] -> Tsil a
{-# INLINE reverseIn #-}
reverseIn = Tsil

-- | Write the contents of the `Tsil` into an array, assuming you know the length of the array.
-- This is useful in the common case of buffering an unknown-length stream before allocating contiguous space for the elements.
--
-- If you sepcify to small a langth, the initial part of the array will be uninitialized.
-- If you specify to large a length, the initial part of the list will not be written.
--
-- If you are unaware of the size of the list, `Arr.fromList . fromList` will do the trick, but will obviously be slower.
toArrayN :: (Contiguous arr, Element arr a) => Int -> Tsil a -> arr a
{-# INLINE toArrayN #-} -- use inline instead of inlinable, because inlinable with Contiguous is busted
{-# SPECIALIZE toArrayN :: Int -> Tsil a -> SmallArray a #-}
toArrayN n (Tsil xs0) = Arr.create $ do
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
toSet :: (Ord a) => Tsil a -> Set a
{-# INLINABLE toSet #-}
toSet (Tsil xs) = Set.fromList xs
