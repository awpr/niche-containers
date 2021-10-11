# keyed-containers

Variants of `IntSet` and `IntMap` supporting newtyped `Int` keys.

These use [attenuation](https://hackage.haskell.org/package/attenuation) to
accept any key type that can be coerced to `Int`, without requiring that `Int`
can be coerced back.  This means newtypes that impose additional invariants on
`Int` can still be used, as long as they provide `Attenuable` instances.

In particular, this means `Fin` from
[fin-int](https://hackage.haskell.org/package/fin-int) can now be the key of a
map type with `IntMap`'s performance characteristics.
