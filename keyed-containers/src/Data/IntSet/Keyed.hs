-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A wrapper around "Data.IntSet" supporting newtyped 'Int' keys.

module Data.IntSet.Keyed
         ( -- * IntSet
           IntSet
           -- ** Construction
         , empty, singleton
           -- ** Insertion
         , insert
           -- ** Deletion/Update
         , delete, alterF
           -- ** Lookup
           -- *** Exact
         , member, notMember
           -- *** Nearest
           -- $approximate_lookups
         , lookupReprLT, lookupReprGT, lookupReprLE, lookupReprGE
           -- *** By Int Value
           -- $int_lookups
         , lookupLTInt, lookupGTInt, lookupLEInt, lookupGEInt, memberInt
           -- *** Min/Max
         , findMin, findMax
         , deleteMin, deleteMax, deleteFindMin, deleteFindMax
         , minView, maxView
           -- ** Splitting
         , splitRepr, splitInt, splitMemberRepr, splitMemberInt, splitRoot
           -- ** Set Operations
           -- *** Union
         , union, unions
           -- *** Difference
         , difference, (\\)
           -- *** Intersection
         , intersection
           -- *** Subset
         , isSubsetOf, isProperSubsetOf
           -- *** Miscellaneous
         , null, size, disjoint
           -- ** Elementwise Operations
           -- *** Maps
         , map, mapMonotonic
           -- *** Folds
         , foldr, foldl
           -- *** Strict Folds
         , foldr', foldl'
           -- *** Subselection
         , filter, partition
           -- ** Conversion
           -- *** Lists
         , elems, fromList, toList
           -- *** Ordered Lists
           -- $underlying_ints
         , fromAscList, fromDistinctAscList, toAscList, toDescList
         ) where

import Prelude hiding ((.), lookup, null, map, foldr, foldl, filter, traverse)

import Control.Category ((.))
import Data.Coerce (coerce)
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)
import Data.Type.Coercion (gcoerceWith)
import GHC.Stack (HasCallStack)

import Data.Type.Attenuation
         ( type (⊆), Attenuable, Attenuation, Representational
         , attenuate, attenuateWith, attenuation, coercible
         )
import Data.Type.Attenuation.Unsafe (unsafeSym, unsafeToCoercion)

import Data.IntSet.Keyed.Internal

attToInt :: a ⊆ Int => Attenuation a Int
attToInt = attenuation

unsafeAttToK :: a ⊆ Int => Attenuation Int a
unsafeAttToK = unsafeSym attenuation

attenuateIt :: forall k a b. (k ⊆ Int, Int ⊆ k => Attenuable a b) => a -> b
attenuateIt = attenuate \\- unsafeAttToK @k

singleton :: forall a. a ⊆ Int => a -> IntSet a
singleton = attenuateIt @a IS.singleton

-- $underlying_ints
--
-- For these functions, the relevant meaning of "ascending order" is w.r.t. the
-- underlying 'Int' values of the keys.  This means custom 'Ord' instances /are
-- not relevant/ here: 'IntSet' is only concerned with the 'Int' values that
-- represent its keys, and not any user-defined ordering.  In particular, this
-- means if you've 'Data.List.sort'ed your lists by a custom 'Ord' instance
-- that compares differently from @deriving Ord via Int@, they will be in the
-- wrong order for these functions.

-- | Build a set from a list in ascending order /by underlying 'Int'/.
fromAscList :: forall a. a ⊆ Int => [a] -> IntSet a
fromAscList = attenuateIt @a IS.fromAscList

-- | Build a set from a list in ascending order with no duplicate keys.
fromDistinctAscList :: forall a. a ⊆ Int => [a] -> IntSet a
fromDistinctAscList = attenuateIt @a IS.fromDistinctAscList

insert :: forall a. a ⊆ Int => a -> IntSet a -> IntSet a
insert = attenuateIt @a IS.insert

delete :: forall a. a ⊆ Int => a -> IntSet a -> IntSet a
delete = attenuateIt @a IS.delete

-- | Modify a value's membership in the set under an arbitrary 'Functor'.
--
-- For versions of @containers@ prior to 0.6.3, this is less efficient.
alterF
  :: forall f a
   . (Functor f, Representational f, a ⊆ Int)
  => (Bool -> f Bool) -> a -> IntSet a -> f (IntSet a)
#if MIN_VERSION_containers(0, 6, 3)
alterF = attenuateIt @a (IS.alterF @f)
#else
alterF f x m = (\y -> (if y then insert else delete) x m) <$> f (member x m)
#endif

member :: forall a. a ⊆ Int => a -> IntSet a -> Bool
member = attenuateIt @a IS.member

notMember :: forall a. a ⊆ Int => a -> IntSet a -> Bool
notMember = attenuateIt @a IS.notMember

-- $approximate_lookups
--
-- As with the 'fromAscList' family of functions, these functions are concerned
-- only with the ordering of the /underlying 'Int's/, and not with any
-- user-defined 'Ord' instances.  For example, 'lookupReprLT' will return the
-- entry with the key whose 'Int' value is less than the given one and greater
-- than any other, and /not/ the entry whose key 'compare's less than the given
-- one and greater than any other such key.
--
-- These functions are named differently from the corresponding ones in
-- @containers@ intentionally to trip up anyone unwittingly using them without
-- considering this subtlety, and force them to come to the documentation
-- first.

lookupReprLT :: forall a. a ⊆ Int => a -> IntSet a -> Maybe a
lookupReprLT = attenuateIt @a IS.lookupLT

lookupReprGT :: forall a. a ⊆ Int => a -> IntSet a -> Maybe a
lookupReprGT = attenuateIt @a IS.lookupGT

lookupReprLE :: forall a. a ⊆ Int => a -> IntSet a -> Maybe a
lookupReprLE = attenuateIt @a IS.lookupLE

lookupReprGE :: forall a. a ⊆ Int => a -> IntSet a -> Maybe a
lookupReprGE = attenuateIt @a IS.lookupGE

-- $int_lookups
--
-- Since @a@ might be a strict subset of @Int@, it could conceivably be useful
-- to query an 'IntSet' for 'Int' keys that aren't known to be values of @a@
-- without explicitly validating them first.  If they're found, obviously they
-- were valid, and if not, no harm is done.  So, we provide search functions
-- with 'Int' queries.

lookupLTInt :: forall a. a ⊆ Int => Int -> IntSet a -> Maybe a
lookupLTInt = attenuateIt @a IS.lookupLT

lookupGTInt :: forall a. a ⊆ Int => Int -> IntSet a -> Maybe a
lookupGTInt = attenuateIt @a IS.lookupGT

lookupLEInt :: forall a. a ⊆ Int => Int -> IntSet a -> Maybe a
lookupLEInt = attenuateIt @a IS.lookupLE

lookupGEInt :: forall a. a ⊆ Int => Int -> IntSet a -> Maybe a
lookupGEInt = attenuateIt @a IS.lookupGE

memberInt :: forall a. a ⊆ Int => Int -> IntSet a -> Maybe a
memberInt x s = if attenuateIt @a IS.member x s
  then Just (attenuateIt @a x)
  else Nothing

null :: forall a. IntSet a -> Bool
null = coerce IS.null

size :: forall a. IntSet a -> Int
size = coerce IS.size

unions
  :: forall f a
   . (a ⊆ Int, Representational f, Foldable f)
  => f (IntSet a) -> IntSet a
unions = gcoerceWith (unsafeToCoercion (attToInt @a)) $
  attenuate (IS.unions @f)
    -- Eww: have to manually build a 'Coercion' and constrain 'f' to coerce under
    -- it, because it's not a 'Functor'.  Oh well?
    \\- coercible @(f (IntSet a)) @(f IS.IntSet)

-- | Return the first set, less any elements present inthe second set.
(\\), difference
  :: forall a
   . a ⊆ Int
  => IntSet a -> IntSet a -> IntSet a
difference = attenuateIt @a IS.difference
(\\) = difference

intersection
  :: forall a
   . IntSet a -> IntSet a -> IntSet a
intersection = attenuate IS.intersection
  \\- intSetIsNewtype @a

-- | Test whether two sets are disjoint.
--
-- For versions of @containers@ prior to 0.6.2, this computes the intersection
-- and tests whether it's empty.
disjoint :: forall a. IntSet a -> IntSet a -> Bool
#if MIN_VERSION_containers(0, 6, 2)
disjoint = attenuate IS.disjoint
#else
disjoint xs = null . intersection xs
#endif

map :: forall a b. (a ⊆ Int, b ⊆ Int) => (a -> b) -> IntSet a -> IntSet b
map = attenuateIt @a IS.map
  \\- intSetIsNewtype @b

-- | Like 'map', but using a monotonicity invariant to improve performance.
--
-- For versions of @containers@ prior to 0.6.3, this is just 'map'.
mapMonotonic
  :: forall a b
   . (a ⊆ Int, b ⊆ Int)
  => (a -> b) -> IntSet a -> IntSet b
#if MIN_VERSION_containers(0, 6, 3)
mapMonotonic = attenuate IS.mapMonotonic
  \\- unsafeAttToK @a
  \\- unsafeAttToK @b
#else
mapMonotonic = map
#endif

foldr :: forall a b. a ⊆ Int => (a -> b -> b) -> b -> IntSet a -> b
foldr = attenuateIt @a (IS.foldr @b)

foldl :: forall a b. a ⊆ Int => (b -> a -> b) -> b -> IntSet a -> b
foldl = attenuateIt @a (IS.foldl @b)

foldr' :: forall a b. a ⊆ Int => (a -> b -> b) -> b -> IntSet a -> b
foldr' = attenuateIt @a (IS.foldr' @b)

foldl' :: forall a b. a ⊆ Int => (b -> a -> b) -> b -> IntSet a -> b
foldl' = attenuateIt @a (IS.foldl' @b)

elems :: forall a. a ⊆ Int => IntSet a -> [a]
elems = attenuateIt @a IS.elems

toAscList :: forall a. a ⊆ Int => IntSet a -> [a]
toAscList = attenuateIt @a IS.toAscList

toDescList :: forall a. a ⊆ Int => IntSet a -> [a]
toDescList = attenuateIt @a IS.toDescList

filter :: forall a. a ⊆ Int => (a -> Bool) -> IntSet a -> IntSet a
filter = attenuateIt @a IS.filter

partition
  :: forall a
   . a ⊆ Int
  => (a -> Bool) -> IntSet a -> (IntSet a, IntSet a)
partition = attenuateIt @a IS.partition

-- | Split into sets with elements greater and lesser, by the underlying 'Int's.
splitRepr :: forall a. a ⊆ Int => a -> IntSet a -> (IntSet a, IntSet a)
splitRepr = attenuateIt @a IS.split

-- | Like 'splitRepr', but with an 'Int' split point.
splitInt
  :: forall a
   . a ⊆ Int
  => Int -> IntSet a -> (IntSet a, IntSet a)
splitInt = attenuateIt @a IS.split

splitMemberRepr
  :: forall a
   . a ⊆ Int
  => a -> IntSet a -> (IntSet a, Bool, IntSet a)
splitMemberRepr = attenuateIt @a IS.splitMember

-- | Like 'splitMemberRepr', but with an 'Int' split point.
--
-- This form returns the found key as well, having validated that it was an
-- inhabitant of @a@ by virtue of encountering it in the set.
splitMemberInt
  :: forall a
   . a ⊆ Int
  => Int -> IntSet a -> (IntSet a, Maybe a, IntSet a)
splitMemberInt a m =
  let (l, b, r) = attenuateIt @a IS.splitMember a m
  in  (l, if b then Just (attenuateWith unsafeAttToK a) else Nothing, r)

splitRoot :: forall a. a ⊆ Int => IntSet a -> [IntSet a]
splitRoot = attenuateIt @a IS.splitRoot

isSubsetOf :: forall a. IntSet a -> IntSet a -> Bool
isSubsetOf = attenuate IS.isSubsetOf

isProperSubsetOf :: forall a. IntSet a -> IntSet a -> Bool
isProperSubsetOf = attenuate IS.isProperSubsetOf

-- $minmax_underlying_ints
--
-- For these functions, "min" and "max" refer to the underlying 'Int'
-- representation, and not to any user-defined 'Ord'ering.  For example, the
-- element found by 'lookupMin' will be the one with the least 'Int' value in
-- the set, but might 'compare' 'GT' some or all other elements, depending on
-- the key type's 'Ord' instance.

findMin :: forall a. (HasCallStack, a ⊆ Int) => IntSet a -> a
findMin = fst . fromMaybe (error "findMin: empty map") . minView

findMax :: forall a. (HasCallStack, a ⊆ Int) => IntSet a -> a
findMax = fst . fromMaybe (error "findMax: empty map") . maxView

deleteMin :: forall a. a ⊆ Int => IntSet a -> IntSet a
deleteMin = attenuateIt @a IS.deleteMin

deleteMax :: forall a. a ⊆ Int => IntSet a -> IntSet a
deleteMax = attenuateIt @a IS.deleteMax

deleteFindMin
  :: forall a. (HasCallStack, a ⊆ Int) => IntSet a -> (a, IntSet a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map") . minView

deleteFindMax
  :: forall a. (HasCallStack, a ⊆ Int) => IntSet a -> (a, IntSet a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map") . maxView

minView :: forall a. a ⊆ Int => IntSet a -> Maybe (a, IntSet a)
minView = attenuateIt @a IS.minView

maxView :: forall a. a ⊆ Int => IntSet a -> Maybe (a, IntSet a)
maxView = attenuateIt @a IS.maxView
