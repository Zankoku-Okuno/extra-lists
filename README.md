# Variations on the List Theme in Haskell

The builtin Haskell lists have these design choices:
  * codata
  * lazy contents
  * cons-oriented

When these choices are not the correct ones for you application, you need something else.
I want to stop writing the something else over-and-over.

## Usage

This is a monorepo containing cabal packages.
I've not (yet?) hosted the packages on Hackage.
Include the following in your `cabal.project` file:

```
source-repository-package
  type: git
  location: https://github.com/Zankoku-Okuno/extra-lists
  tag: <your choice of commit hash>
  subdir: <name of package>
```

## reverse-list

These are snoc lists.

How many times have you written a recursive function that accumulates a list backwards, and then reverses it?
Now, accumulate an `RList`, and the type system will ensure you convert it (probably by revering) before returning the the list-expecting caller.

Until levity-polymorphic data types make their way into GHC, these have the same laziness choices as vanilla lists.
