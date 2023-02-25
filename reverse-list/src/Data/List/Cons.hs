{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module exists primarily for symmetry with "Data.List.Snoc"
-- However, it can also be used in place of the "Prelude" list type:
--
-- This module only exports functions that are efficient on linked lists. Many
-- functions on that type ('Prelude.last', 'Data.List.isSuffixOf') though
-- technically implementable, do not represent the intended use of a linked list
-- in terms of performance.
--
-- Additionally, this module does not export any partial functions: 'head' and
-- 'tail' return their results under a 'Maybe'.
module Data.List.Cons
  ( List
  , pattern Nil
  , pattern Cons
  , nil
  , cons
  , uncons
  , singleton
  , head
  , tail
  ) where

import Prelude hiding (head,tail)

-- | As a counterpart to 'Data.List.Snoc.Tsil'.
type List = ([])

{-# COMPLETE Nil, Cons #-}

-- | An empty 'List', such as 'nil'.
pattern Nil :: List a
pattern Nil = []

-- | The 'List' consisting of head and tail elements, such as created by 'cons'.
pattern Cons :: a -> List a -> List a
pattern Cons x xs <- (uncons -> Just (x, xs))
  where Cons = cons

-- | The empty 'List'.
nil :: List a
{-# INLINABLE nil #-}
nil = []

-- | @O(1)@ Append an element.
--
-- If you are looking for @snoc@, you should use an 'Data.List.Snoc.Tsil', or a finite sequence/queue type.
cons :: a -> List a -> List a
{-# INLINABLE cons #-}
cons = (:)

-- | @O(1)@ Access the first element and trailing portion of the list.
-- See also 'head' and 'tail' if you only need one component.
--
-- If you are looking for @unsnoc@, you should use an 'Data.List.Snoc.Tsil', or a finite sequence/queue type.
uncons :: List a -> Maybe (a, List a)
{-# INLINABLE uncons #-}
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

-- | @O(1)@ extract the first element of a list, if it exists.
-- See also 'uncons' if you also need 'tail' at the same time.
head :: List a -> Maybe a
{-# INLINABLE head #-}
head Nil = Nothing
head (Cons x _) = Just x

-- | @O(1)@ extract the elements of a list other than the last, if they exist.
-- See also 'uncons' if you also need 'head' at the same time.
tail :: List a -> Maybe (List a)
{-# INLINABLE tail #-}
tail Nil = Nothing
tail (Cons _ xs) = Just xs

-- | Create a single-element 'List'.
singleton :: a -> List a
{-# INLINE singleton #-}
singleton = (:[])
