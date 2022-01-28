# reverse-list

The key idea of this library is to leverage the type system to control the performance characteristics of list-manipulation code.
It defines the type `RList`, which is a snoc-list rather than a cons-list.
It also creates a symmetric module for cons-lists, which focuses on the efficient and safe use of linked lists.

Admittedly, parsing `String`s as in this example is bad for performance anyway, but the potential bugs are the same for any use of lists as accumulators:
```
import qualified Data.List.Snoc as RList

parseSqlString :: String -> Maybe String
parseSqlString str0 = case str0 of
  '\'':rest -> loop "" rest
  _ -> Nothing
  where
  loop :: RList Char -> [Char] -> Maybe [Char]
  loop acc [] = Nothing
  -- it is impossible to accidentally return the accumulator without reversing
  loop acc "\'" = Just $ Rlist.toList acc
  loop acc ('\'':'\'':rest) = loop (Snoc acc '\'') rest
  loop acc (c:rest) = loop (Snoc acc c) rest
```

Currently, we only support the basic introduction/elimination forms (though reasonably ergonomically), and conversions.
Additional functions should certainly be exposed, after due consideration is given to their semantics, including performance.
If you run into anything you think deserved to be exported, open an issue or a pull request and I'll be happy to get it done.
