-- Copyright 2021 Google LLC
-- Copyright 2023 Andrew Pritchard
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

-- Since the APIs between lazy and strict IntMaps are identical, I'm
-- deduplicating code by #including it.  Maybe I should learn about backpack
-- someday...

{-# LANGUAGE AllowAmbiguousTypes #-}
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

module MODULE_NAME
         ( -- * IntMap
           IntMap
           -- ** Construction
         , empty, singleton, fromSet
           -- ** Insertion
         , insert, insertS, insertWith, insertWithKey, insertLookupWithKey
           -- ** Deletion/Update
         , delete
         , adjust, adjustWithKey
         , update, updateWithKey, updateLookupWithKey
         , alter, alterF
           -- ** Lookup
           -- *** Exact
         , lookup, (!?), (!), findWithDefault, member, notMember
           -- *** Nearest
           -- $approximate_lookups
         , lookupReprLT, lookupReprGT, lookupReprLE, lookupReprGE
           -- *** By Int Value
           -- $int_lookups
         , lookupLTInt, lookupGTInt, lookupLEInt, lookupGEInt
         , lookupInt, findWithDefaultInt, memberInt
           -- *** Min/Max
         , lookupMin, lookupMax
         , findMin, findMax
         , deleteMin, deleteMax, deleteFindMin, deleteFindMax
         , updateMin, updateMax, updateMinWithKey, updateMaxWithKey
         , minView, maxView, minViewWithKey, maxViewWithKey
           -- ** Splitting
         , splitRepr, splitInt, splitLookupRepr, splitLookupInt, splitRoot
           -- ** Set Operations
           -- *** Union
         , union, unionWith, unionWithKey, unions, unionsWith
           -- *** Difference
         , difference, (\\), differenceWith, differenceWithKey
         , withoutKeys
           -- *** Intersection
         , intersection, intersectionWith, intersectionWithKey
         , restrictKeys
           -- *** Submap
         , isSubmapOf, isSubmapOfBy, isProperSubmapOf, isProperSubmapOfBy
           -- *** Miscellaneous
         , disjoint, null, size
           -- *** Merge
         , mergeWithKey
           -- ** Elementwise Operations
           -- *** Traversals/Maps
         , map, mapWithKey
         , traverse, traverseWithKey, traverseMaybeWithKey
         , traverseNoRep, traverseWithKeyNoRep, traverseMaybeWithKeyNoRep
         , mapAccum, mapAccumWithKey, mapAccumRWithKey
         , mapKeys, mapKeysS, mapKeysWith, mapKeysMonotonic
           -- *** Folds
         , foldr, foldl, foldrWithKey, foldlWithKey, foldMapWithKey
           -- *** Strict Folds
         , foldr', foldl', foldrWithKey', foldlWithKey'
           -- *** Subselection
         , filter, filterWithKey
         , partition, partitionWithKey
         , mapMaybe, mapMaybeWithKey
         , mapEither, mapEitherWithKey
           -- ** Conversion
         , elems, keys, keysSet, assocs
           -- *** Lists
         , fromList, fromListS, fromDistinctList, fromListWith, fromListWithKey
         , toList
           -- *** Ordered Lists
           -- $underlying_ints
         , fromAscList, fromAscListWith, fromAscListWithKey, fromDistinctAscList
         , toAscList, toDescList
           -- ** Compose
         , compose
         ) where

import Prelude hiding ((.), lookup, null, map, foldr, foldl, filter, traverse)
import qualified Prelude (traverse)

import Control.Category ((.))
import Data.Coerce (coerce)
import qualified IMPL_MODULE as IM
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(stimes))
import Data.Type.Coercion (gcoerceWith)
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
import GHC.Stack (HasCallStack)

import Control.DeepSeq (NFData)
import Data.Foldable.WithIndex (FoldableWithIndex(..))
import Data.Functor.WithIndex (FunctorWithIndex(..))
import Data.Portray (Portray)
import Data.Traversable.WithIndex (TraversableWithIndex(..))
import Data.Type.Attenuation
         ( type (⊆), Attenuable, Attenuation, Representational
         , attenuate, attenuateWith, attenuation, coercible
#if MIN_VERSION_containers(0, 6, 4)
         , domain
#endif
         )
import Data.Type.Attenuation.Unsafe (unsafeSym, unsafeToCoercion)
import Data.Wrapped (Wrapped(..))

import Data.IntSet.Keyed.Internal (IntSet, intSetIsNewtype, (\\-))

-- | A map type that uses 'IM.IntMap' internally to store newtyped keys.
newtype IntMap k a = IntMap
  { getIntMap :: IM.IntMap a
    -- ^ Invariant: contains only keys valid for coercion to @k@.
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Foldable, NFData)
  -- Take care: don't derive Traversable or Functor, because the strict version
  -- (which, in Data.IntMap, /is/ the lazy version) still has lazy instances
  -- for those.

instance Functor (IntMap k) where
  fmap = map

instance Traversable (IntMap k) where
  traverse = traverseNoRep

instance k ⊆ Int => FunctorWithIndex k (IntMap k) where
  imap = mapWithKey

instance k ⊆ Int => FoldableWithIndex k (IntMap k) where
  ifoldMap = foldMapWithKey
  ifoldr = foldrWithKey
  ifoldl = foldlWithKey . flip
  ifoldr' = foldrWithKey'
  ifoldl' = foldlWithKey' . flip

instance k ⊆ Int => TraversableWithIndex k (IntMap k) where
  itraverse = traverseWithKeyNoRep

-- | Union of two maps, combining with the element type's ('<>').
--
-- Note this is different from the instance for 'IM.IntMap', which is
-- equivalent to @'IntMap' k ('Data.Semigroup.First' a)@.
instance (k ⊆ Int, Semigroup a) => Semigroup (IntMap k a) where
  (<>) = unionWith (<>)
  stimes n = fmap (stimes n)

instance (k ⊆ Int, Semigroup a) => Monoid (IntMap k a) where
  mempty = empty

deriving via Wrapped IsList (IntMap k a)
  instance (Portray k, Portray a, k ⊆ Int) => Portray (IntMap k a)

instance k ⊆ Int => IsList (IntMap k a) where
  type Item (IntMap k a) = (k, a)
  fromList = fromList
  toList = toList

type role IntMap representational representational

-- | Attenuations between 'IntMap's are fine if keys and values are attenuable.
instance (k ⊆ j, a ⊆ b) => Attenuable (IntMap k a) (IntMap j b) where
  attenuation =
    coercible @(IM.IntMap b) @(IntMap j b) . attenuation

-- | Attenuations to raw 'IM.IntMap' are fine if values are attenuable.
instance a ⊆ b => Attenuable (IntMap k a) (IM.IntMap b) where
  -- Note because the underlying IntMap stores Ints regardless of anything
  -- we're capable of doing in this module, we don't need to know k ⊆ Int to
  -- coerce safely to the underlying IntMap: there aren't /actually/ any @k@s
  -- being coerced when doing that, we're just forgetting that we intended to
  -- interpret some Ints as ks.
  attenuation = attenuation . coercible @(IntMap k a) @(IM.IntMap a)

-- | Attenuations from raw 'IM.IntMap' are fine for types attenuable /from/ Int.
instance (Int ⊆ k, a ⊆ b) => Attenuable (IM.IntMap a) (IntMap k b) where
  attenuation = coercible @(IM.IntMap b) @(IntMap k b) . attenuation

attToInt :: k ⊆ Int => Attenuation k Int
attToInt = attenuation

intMapIsNewtype :: forall k a. Attenuation (IM.IntMap a) (IntMap k a)
intMapIsNewtype = coercible

toInt :: k ⊆ Int => k -> Int
toInt = attenuate

unsafeAttToK :: k ⊆ Int => Attenuation Int k
unsafeAttToK = unsafeSym attenuation

attenuateIt :: forall k a b. (k ⊆ Int, Int ⊆ k => Attenuable a b) => a -> b
attenuateIt = attenuate \\- unsafeAttToK @k

empty :: IntMap k a
empty = IntMap IM.empty

singleton :: forall k a. k ⊆ Int => k -> a -> IntMap k a
singleton = attenuateIt @k (IM.singleton @a)

fromSet :: forall k a. k ⊆ Int => (k -> a) -> IntSet k -> IntMap k a
fromSet = attenuateIt @k (IM.fromSet @a)

-- | Build a map from a list of pairs, replacing earlier duplicate keys.
fromList :: forall k a. k ⊆ Int => [(k, a)] -> IntMap k a
fromList = attenuateIt @k (IM.fromList @a)

-- Like 'fromList', but combining duplicate keys' values with ('<>').
fromListS :: forall k a. (k ⊆ Int, Semigroup a) => [(k, a)] -> IntMap k a
fromListS = fromListWith (<>)

-- | Like 'fromList', but 'error' on duplicated keys.
fromDistinctList
  :: forall k a
   . (HasCallStack, k ⊆ Int)
  => [(k, a)] -> IntMap k a
fromDistinctList = fromListWithKey
  (\k -> error $
    "fromDistinctList: duplicate key " ++
    shows (toInt k) " in list!")

fromListWith :: forall k a. k ⊆ Int => (a -> a -> a) -> [(k, a)] -> IntMap k a
fromListWith = attenuateIt @k (IM.fromListWith @a)

fromListWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> a -> a) -> [(k, a)] -> IntMap k a
fromListWithKey = attenuateIt @k (IM.fromListWithKey @a)

-- $underlying_ints
--
-- For these functions, the relevant meaning of "ascending order" is w.r.t. the
-- underlying 'Int' values of the keys.  This means custom 'Ord' instances /are
-- not relevant/ here: 'IntMap' is only concerned with the 'Int' values that
-- represent its keys, and not any user-defined ordering.  In particular, this
-- means if you've 'Data.List.sort'ed your assoc-lists by a custom 'Ord'
-- instance that compares differently from @deriving Ord via Int@, they will be
-- in the wrong order for these functions.

-- | Build a map from a list of pairs in ascending order /by underlying 'Int'/.
fromAscList :: forall k a. k ⊆ Int => [(k, a)] -> IntMap k a
fromAscList = attenuateIt @k (IM.fromAscList @a)

-- | Build from an ascending list of pairs; combine duplicates with a function.
fromAscListWith
  :: forall k a
   . k ⊆ Int
  => (a -> a -> a) -> [(k, a)] -> IntMap k a
fromAscListWith = attenuateIt @k (IM.fromAscListWith @a)

-- | Build from an ascending list of pairs; combine duplicates with a function.
fromAscListWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> a -> a) -> [(k, a)] -> IntMap k a
fromAscListWithKey = attenuateIt @k (IM.fromAscListWithKey @a)

-- | Build a map from a list of pairs in ascending order with no duplicate keys.
fromDistinctAscList :: forall k a. k ⊆ Int => [(k, a)] -> IntMap k a
fromDistinctAscList = attenuateIt @k (IM.fromDistinctAscList @a)

insert :: forall k a. k ⊆ Int => k -> a -> IntMap k a -> IntMap k a
insert = attenuateIt @k (IM.insert @a)

-- | 'insert', but combine any existing value with ('<>').
insertS
  :: forall k a
   . (k ⊆ Int, Semigroup a)
  => k -> a -> IntMap k a -> IntMap k a
insertS = insertWith (<>)

insertWith
  :: forall k a
   . k ⊆ Int
  => (a -> a -> a) -> k -> a -> IntMap k a -> IntMap k a
insertWith = attenuateIt @k (IM.insertWith @a)

-- | Like 'insertWith', but the combining function has access to the key.
--
-- Compared to 'insertWith', this doesn't add any new capabilities, but this
-- form can be more convenient in some scenarios, like point-free code or
-- partial application to only the combining function argument, and avoids
-- building a closure at runtime to capture the key.
insertWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> a -> a) -> k -> a -> IntMap k a -> IntMap k a
insertWithKey = attenuateIt @k (IM.insertWithKey @a)

insertLookupWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> a -> a) -> k -> a -> IntMap k a -> (Maybe a, IntMap k a)
insertLookupWithKey = attenuateIt @k (IM.insertLookupWithKey @a)

delete :: forall k a. k ⊆ Int => k -> IntMap k a -> IntMap k a
delete = attenuateIt @k (IM.delete @a)

adjust :: forall k a. k ⊆ Int => (a -> a) -> k -> IntMap k a -> IntMap k a
adjust = attenuateIt @k (IM.adjust @a)

adjustWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> a) -> k -> IntMap k a -> IntMap k a
adjustWithKey = attenuateIt @k (IM.adjustWithKey @a)

update
  :: forall k a
   . k ⊆ Int
  => (a -> Maybe a) -> k -> IntMap k a -> IntMap k a
update = attenuateIt @k (IM.update @a)

updateWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> Maybe a) -> k -> IntMap k a -> IntMap k a
updateWithKey = attenuateIt @k (IM.updateWithKey @a)

updateLookupWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> Maybe a) -> k -> IntMap k a -> (Maybe a, IntMap k a)
updateLookupWithKey = attenuateIt @k (IM.updateLookupWithKey @a)

alter
  :: forall k a
   . k ⊆ Int
  => (Maybe a -> Maybe a) -> k -> IntMap k a -> IntMap k a
alter = attenuateIt @k (IM.alter @a)

alterF
  :: forall f k a
   . (Functor f, Representational f, k ⊆ Int)
  => (Maybe a -> f (Maybe a)) -> k -> IntMap k a -> f (IntMap k a)
alterF = attenuateIt @k (IM.alterF @f @a)

lookup :: forall k a. k ⊆ Int => k -> IntMap k a -> Maybe a
lookup = attenuateIt @k (IM.lookup @a)

(!?) :: forall k a. k ⊆ Int => IntMap k a -> k -> Maybe a
(!?) = flip lookup

(!) :: forall k a. (HasCallStack, k ⊆ Int) => IntMap k a -> k -> a
m ! k =
  -- Note: the @containers@ impl doesn't use 'HasCallStack', so we have to
  -- build our own error.
  fromMaybe
    (error $ "Data.IntMap.Keyed.!: missing key " ++ shows (toInt k) "!")
    (lookup k m)

findWithDefault :: forall k a. k ⊆ Int => a -> k -> IntMap k a -> a
findWithDefault = attenuateIt @k (IM.findWithDefault @a)

member :: forall k a. k ⊆ Int => k -> IntMap k a -> Bool
member = attenuateIt @k (IM.member @a)

notMember :: forall k a. k ⊆ Int => k -> IntMap k a -> Bool
notMember = attenuateIt @k (IM.notMember @a)

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

lookupReprLT :: forall k a. k ⊆ Int => k -> IntMap k a -> Maybe (k, a)
lookupReprLT = attenuateIt @k (IM.lookupLT @a)

lookupReprGT :: forall k a. k ⊆ Int => k -> IntMap k a -> Maybe (k, a)
lookupReprGT = attenuateIt @k (IM.lookupGT @a)

lookupReprLE :: forall k a. k ⊆ Int => k -> IntMap k a -> Maybe (k, a)
lookupReprLE = attenuateIt @k (IM.lookupLE @a)

lookupReprGE :: forall k a. k ⊆ Int => k -> IntMap k a -> Maybe (k, a)
lookupReprGE = attenuateIt @k (IM.lookupGE @a)

-- $int_lookups
--
-- Since @k@ might be a strict subset of @Int@, it could conceivably be useful
-- to query an 'IntMap' for 'Int' keys that aren't known to be values of @k@
-- without explicitly validating them first.  If they're found, obviously they
-- were valid, and if not, no harm is done.  So, we provide search functions
-- with 'Int' queries.

lookupLTInt :: forall k a. k ⊆ Int => Int -> IntMap k a -> Maybe (k, a)
lookupLTInt = attenuateIt @k (IM.lookupLT @a)

lookupGTInt :: forall k a. k ⊆ Int => Int -> IntMap k a -> Maybe (k, a)
lookupGTInt = attenuateIt @k (IM.lookupGT @a)

lookupLEInt :: forall k a. k ⊆ Int => Int -> IntMap k a -> Maybe (k, a)
lookupLEInt = attenuateIt @k (IM.lookupLE @a)

lookupGEInt :: forall k a. k ⊆ Int => Int -> IntMap k a -> Maybe (k, a)
lookupGEInt = attenuateIt @k (IM.lookupGE @a)

-- | Like 'lookup', but on plain 'Int' keys.
--
-- On success, returns both the found @k@ key and the element.
lookupInt :: forall k a. k ⊆ Int => Int -> IntMap k a -> Maybe (k, a)
lookupInt k m = (attenuateIt @k k,) <$> attenuateIt @k (IM.lookup @a) k m

-- | Like 'findWithDefault', but on plain 'Int' keys.
--
-- Unlike 'lookupInt', this doesn't necessarily find a @k@ key for the given
-- 'Int', so it returns only the element or default value.
findWithDefaultInt :: forall k a. a -> Int -> IntMap k a -> a
findWithDefaultInt = attenuate (IM.findWithDefault @a)

-- | Like 'member', but on plain 'Int' keys.
--
-- On success, returns 'Just' the found @k@.
memberInt :: forall k a. k ⊆ Int => k -> IntMap k a -> Maybe k
-- GHC 9.0 does something weird with this and ends up picking an instance that
-- wants Coercible, unless we point it at @k@.  No idea why.
memberInt k m = if attenuateIt @k @_ @(k -> _) (IM.member @a) k m
  then Just (attenuateIt @k k)
  else Nothing

null :: forall k a. IntMap k a -> Bool
null = coerce (IM.null @a)

size :: forall k a. IntMap k a -> Int
size = coerce (IM.size @a)

union :: forall k a. k ⊆ Int => IntMap k a -> IntMap k a -> IntMap k a
union = attenuateIt @k (IM.union @a)

unionWith
  :: forall k a
   . k ⊆ Int
  => (a -> a -> a) -> IntMap k a -> IntMap k a -> IntMap k a
unionWith = attenuateIt @k (IM.unionWith @a)

unionWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> a -> a) -> IntMap k a -> IntMap k a -> IntMap k a
unionWithKey = attenuateIt @k (IM.unionWithKey @a)

unions
  :: forall f k a
   . (k ⊆ Int, Representational f, Foldable f)
  => f (IntMap k a) -> IntMap k a
unions = gcoerceWith (unsafeToCoercion (attToInt @k)) $
  attenuate (IM.unions @f @a)
    -- Eww: have to manually build a 'Coercion' and constrain 'f' to coerce
    -- under it.  Oh well?
    \\- coercible @(f (IntMap k a)) @(f (IM.IntMap a))

unionsWith
  :: forall f k a
   . (k ⊆ Int, Representational f, Foldable f)
  => (a -> a -> a) -> f (IntMap k a) -> IntMap k a
unionsWith = gcoerceWith (unsafeToCoercion (attToInt @k)) $
  attenuate (IM.unionsWith @f @a)
    \\- coercible @(f (IntMap k a)) @(f (IM.IntMap a))

-- | Return the first map, less any entries for keys present in the second map.
(\\), difference
  :: forall k a b
   . k ⊆ Int
  => IntMap k a -> IntMap k b -> IntMap k a
difference = attenuateIt @k (IM.difference @a @b)
(\\) = difference

differenceWith
  :: forall k a b
   . k ⊆ Int
  => (a -> b -> Maybe a) -> IntMap k a -> IntMap k b -> IntMap k a
differenceWith = attenuateIt @k (IM.differenceWith @a @b)

differenceWithKey
  :: forall k a b
   . k ⊆ Int
  => (k -> a -> b -> Maybe a) -> IntMap k a -> IntMap k b -> IntMap k a
differenceWithKey = attenuateIt @k (IM.differenceWithKey @a @b)

intersection :: forall k a b. IntMap k a -> IntMap k b -> IntMap k a
intersection = attenuate (IM.intersection @a @b)
  \\- intMapIsNewtype @k @a
  \\- intMapIsNewtype @k @b

intersectionWith
  :: forall k a b c
   . k ⊆ Int
  => (a -> b -> c) -> IntMap k a -> IntMap k b -> IntMap k c
intersectionWith = attenuateIt @k (IM.intersectionWith @a @b @c)

intersectionWithKey
  :: forall k a b c
   . k ⊆ Int
  => (k -> a -> b -> c) -> IntMap k a -> IntMap k b -> IntMap k c
intersectionWithKey = attenuateIt @k (IM.intersectionWithKey @a @b @c)

-- | Tests whether two maps have disjoint keys.
--
-- For versions of @containers@ prior to 0.6.2, this computes the intersection
-- and tests whether it's empty.
disjoint :: forall k a b. IntMap k a -> IntMap k b -> Bool
#if MIN_VERSION_containers(0, 6, 2)
disjoint = attenuate (IM.disjoint @a @b)
#else
disjoint xs = null . intersection xs
#endif

-- | Like 'IM.compose'.
--
-- Note this could be more polymorphic: the element type of the second map
-- doesn't /have/ to be any particular type: it's all 'Int's under the hood,
-- and we can safely try to query any 'IntMap' for any 'Int'.  For now, it's
-- favoring type inference convenience.
--
-- For versions of @containers@ prior 0.6.3, this is less efficient.
compose
  :: forall k j a
   . (k ⊆ Int, j ⊆ Int)
  => IntMap k a -> IntMap j k -> IntMap j a
#if MIN_VERSION_containers(0, 6, 3)
compose = attenuateIt @j (IM.compose @a)
#else
compose ja = mapMaybe (flip lookup ja)
#endif

mergeWithKey
  :: forall k a b c
   . k ⊆ Int
  => (k -> a -> b -> Maybe c)
  -> (IntMap k a -> IntMap k c)
  -> (IntMap k b -> IntMap k c)
  -> IntMap k a -> IntMap k b -> IntMap k c
mergeWithKey = attenuateIt @k (IM.mergeWithKey @a @b @c)

map :: forall k a b. (a -> b) -> IntMap k a -> IntMap k b
map = attenuate (IM.map @a @b)
  \\- intMapIsNewtype @k @b

mapWithKey :: forall k a b. k ⊆ Int => (k -> a -> b) -> IntMap k a -> IntMap k b
mapWithKey = attenuateIt @k (IM.mapWithKey @a @b)

-- | Traverse the elements of a map.
--
-- Unlike most traversal functions, this expects an extra 'Representational'
-- constraint on @f@, so it can be a pure coercion of the underlying traversal
-- of 'IM.IntMap', and not add an extra 'fmap' to convert the result.
--
-- For any specific type @f@ with an 'Applicative' instance, it will almost
-- certainly satisfy 'Representational'.  For polymorphic contexts where the
-- constraint is unavailable or too annoying to plumb through the type
-- signatures, 'traverseNoRep' is a variant that doesn't require it.
traverse
  :: forall f k a b
   . (Applicative f, Representational f)
  => (a -> f b) -> IntMap k a -> f (IntMap k b)
traverse = attenuate (IM.traverseWithKey @f @a @b . const)
  \\- intMapIsNewtype @k @a
  \\- intMapIsNewtype @k @b

-- | 'traverse' with only 'Applicative' required of @f@.
--
-- This costs an extra 'fmap' over the final result compared to 'traverse', but
-- doesn't require @f@ to be 'Representational', which can be useful in some
-- polymorphic contexts such as in instance definitions of 'Traversable'.
traverseNoRep
  :: forall f k a b
   . Applicative f
  => (a -> f b) -> IntMap k a -> f (IntMap k b)
traverseNoRep f (IntMap m) =
  IntMap <$> IM.traverseWithKey (const f) m

traverseWithKey
  :: forall f k a b
   . (k ⊆ Int, Applicative f, Representational f)
  => (k -> a -> f b) -> IntMap k a -> f (IntMap k b)
traverseWithKey = attenuate (IM.traverseWithKey @f @a @b)
  \\- unsafeAttToK @k

traverseWithKeyNoRep
  :: forall f k a b
   . (k ⊆ Int, Applicative f)
  => (k -> a -> f b) -> IntMap k a -> f (IntMap k b)
traverseWithKeyNoRep f (IntMap m) =
  IntMap <$> IM.traverseWithKey (f . attenuateIt @k) m

-- | Traverse the map with access to the key, possibly removing elements.
--
-- For versions of @containers@ prior to 0.6.4, this is less efficient.
traverseMaybeWithKey
  :: forall f k a b
   . (k ⊆ Int, Applicative f, Representational f)
  => (k -> a -> f (Maybe b)) -> IntMap k a -> f (IntMap k b)
#if MIN_VERSION_containers(0, 6, 4)
traverseMaybeWithKey = attenuateIt @k (IM.traverseMaybeWithKey @f @a @b)
#else
traverseMaybeWithKey f = fmap (mapMaybe id) . traverseWithKey f
#endif

-- | Traverse the map with access to the key, possibly removing elements.
--
-- For versions of @containers@ prior to 0.6.4, this is less efficient.
traverseMaybeWithKeyNoRep
  :: forall f k a b
   . (k ⊆ Int, Applicative f)
  => (k -> a -> f (Maybe b)) -> IntMap k a -> f (IntMap k b)
#if MIN_VERSION_containers(0, 6, 4)
traverseMaybeWithKeyNoRep f (IntMap m) =
  IntMap <$>
  IM.traverseMaybeWithKey (attenuateWith (domain (unsafeAttToK @k)) f) m
#else
traverseMaybeWithKeyNoRep f = fmap (mapMaybe id) . traverseWithKeyNoRep f
#endif

mapAccum
  :: forall k a b c
   . k ⊆ Int
  => (a -> b -> (a, c)) -> a -> IntMap k b -> (a, IntMap k c)
mapAccum = attenuateIt @k (IM.mapAccum @a @b @c)

mapAccumWithKey
  :: forall k a b c
   . k ⊆ Int
  => (a -> k -> b -> (a, c)) -> a -> IntMap k b -> (a, IntMap k c)
mapAccumWithKey = attenuateIt @k (IM.mapAccumWithKey @a @b @c)

mapAccumRWithKey
  :: forall k a b c
   . k ⊆ Int
  => (a -> k -> b -> (a, c)) -> a -> IntMap k b -> (a, IntMap k c)
mapAccumRWithKey = attenuateIt @k (IM.mapAccumRWithKey @a @b @c)

mapKeys
  :: forall k j a
   . (k ⊆ Int, j ⊆ Int)
  => (k -> j) -> IntMap k a -> IntMap j a
mapKeys = attenuate (IM.mapKeys @a)
  \\- unsafeAttToK @k
  \\- unsafeAttToK @j

mapKeysS
  :: forall k j a
   . (k ⊆ Int, j ⊆ Int, Semigroup a)
  => (k -> j) -> IntMap k a -> IntMap j a
mapKeysS = mapKeysWith (<>)

mapKeysWith
  :: forall k j a
   . (k ⊆ Int, j ⊆ Int)
  => (a -> a -> a) -> (k -> j) -> IntMap k a -> IntMap j a
mapKeysWith = attenuate (IM.mapKeysWith @a)
  \\- unsafeAttToK @k
  \\- unsafeAttToK @j

mapKeysMonotonic
  :: forall k j a
   . (k ⊆ Int, j ⊆ Int)
  => (k -> j) -> IntMap k a -> IntMap j a
mapKeysMonotonic = attenuate (IM.mapKeysMonotonic @a)
  \\- unsafeAttToK @k
  \\- unsafeAttToK @j

foldr :: forall k a b. (a -> b -> b) -> b -> IntMap k a -> b
foldr = attenuate (IM.foldr @a @b)

foldl :: forall k a b. (b -> a -> b) -> b -> IntMap k a -> b
foldl = attenuate (IM.foldl @b @a)

foldrWithKey
  :: forall k a b
   . k ⊆ Int
  => (k -> a -> b -> b) -> b -> IntMap k a -> b
foldrWithKey = attenuateIt @k (IM.foldrWithKey @a @b)

foldlWithKey
  :: forall k a b
   . k ⊆ Int
  => (b -> k -> a -> b) -> b -> IntMap k a -> b
foldlWithKey = attenuateIt @k (IM.foldlWithKey @b @a)

foldMapWithKey
  :: forall k m a
   . (k ⊆ Int, Monoid m)
  => (k -> a -> m) -> IntMap k a -> m
foldMapWithKey = attenuateIt @k (IM.foldMapWithKey @m @a)

foldr' :: forall k a b. (a -> b -> b) -> b -> IntMap k a -> b
foldr' = attenuate (IM.foldr' @a @b)

foldl' :: forall k a b. (b -> a -> b) -> b -> IntMap k a -> b
foldl' = attenuate (IM.foldl' @b @a)

foldrWithKey'
  :: forall k a b
   . k ⊆ Int
  => (k -> a -> b -> b) -> b -> IntMap k a -> b
foldrWithKey' = attenuateIt @k (IM.foldrWithKey' @a @b)

foldlWithKey'
  :: forall k a b
   . k ⊆ Int
  => (b -> k -> a -> b) -> b -> IntMap k a -> b
foldlWithKey' = attenuateIt @k (IM.foldlWithKey' @b @a)


elems :: forall k a. IntMap k a -> [a]
elems = attenuate (IM.elems @a)

keys :: forall k a. k ⊆ Int => IntMap k a -> [k]
keys = attenuateIt @k (IM.keys @a)

assocs :: forall k a. k ⊆ Int => IntMap k a -> [(k, a)]
assocs = toAscList

keysSet :: forall k a. IntMap k a -> IntSet k
keysSet = attenuate (IM.keysSet @a)
  \\- intMapIsNewtype @k @a
  \\- intSetIsNewtype @k

toList :: forall k a. k ⊆ Int => IntMap k a -> [(k, a)]
toList = attenuateIt @k (IM.toList @a)

toAscList :: forall k a. k ⊆ Int => IntMap k a -> [(k, a)]
toAscList = attenuateIt @k (IM.toAscList @a)

toDescList :: forall k a. k ⊆ Int => IntMap k a -> [(k, a)]
toDescList = attenuateIt @k (IM.toDescList @a)

filter :: forall k a. k ⊆ Int => (a -> Bool) -> IntMap k a -> IntMap k a
filter = attenuateIt @k (IM.filter @a)

filterWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> Bool) -> IntMap k a -> IntMap k a
filterWithKey = attenuateIt @k (IM.filterWithKey @a)

restrictKeys :: forall k a. IntMap k a -> IntSet k -> IntMap k a
restrictKeys = attenuate (IM.restrictKeys @a)
  \\- intMapIsNewtype @k @a
  \\- intSetIsNewtype @k

withoutKeys :: forall k a. IntMap k a -> IntSet k -> IntMap k a
withoutKeys = attenuate (IM.withoutKeys @a)
  \\- intMapIsNewtype @k @a
  \\- intSetIsNewtype @k

partition
  :: forall k a
   . k ⊆ Int
  => (a -> Bool) -> IntMap k a -> (IntMap k a, IntMap k a)
partition = attenuateIt @k (IM.partition @a)

partitionWithKey
  :: forall k a
   . k ⊆ Int
  => (k -> a -> Bool) -> IntMap k a -> (IntMap k a, IntMap k a)
partitionWithKey = attenuateIt @k (IM.partitionWithKey @a)

mapMaybe
  :: forall k a b
   . k ⊆ Int
  => (a -> Maybe b) -> IntMap k a -> IntMap k b
mapMaybe = attenuateIt @k (IM.mapMaybe @a @b)

mapMaybeWithKey
  :: forall k a b
   . k ⊆ Int
  => (k -> a -> Maybe b) -> IntMap k a -> IntMap k b
mapMaybeWithKey = attenuateIt @k (IM.mapMaybeWithKey @a @b)

mapEither
  :: forall k a b c
   . k ⊆ Int
  => (a -> Either b c) -> IntMap k a -> (IntMap k b, IntMap k c)
mapEither = attenuateIt @k (IM.mapEither @a @b @c)

mapEitherWithKey
  :: forall k a b c
   . k ⊆ Int
  => (k -> a -> Either b c) -> IntMap k a -> (IntMap k b, IntMap k c)
mapEitherWithKey = attenuateIt @k (IM.mapEitherWithKey @a @b @c)

-- | Split into maps with keys greater and lesser, by the underlying 'Int's.
splitRepr :: forall k a. k ⊆ Int => k -> IntMap k a -> (IntMap k a, IntMap k a)
splitRepr = attenuateIt @k (IM.split @a)

-- | Like 'splitRepr', but with an 'Int' split point.
splitInt
  :: forall k a
   . k ⊆ Int
  => Int -> IntMap k a -> (IntMap k a, IntMap k a)
splitInt = attenuateIt @k (IM.split @a)

splitLookupRepr
  :: forall k a
   . k ⊆ Int
  => k -> IntMap k a -> (IntMap k a, Maybe a, IntMap k a)
splitLookupRepr = attenuateIt @k (IM.splitLookup @a)

-- | Like 'splitLookupRepr', but with an 'Int' split point.
--
-- This form returns the found key as well, having validated that it was an
-- inhabitant of @k@ by virtue of encountering it in the map.
splitLookupInt
  :: forall k a
   . k ⊆ Int
  => Int -> IntMap k a -> (IntMap k a, Maybe (k, a), IntMap k a)
splitLookupInt k m =
  let (l, x, r) = attenuateIt @k (IM.splitLookup @a) k m
  in  (l, (attenuateWith unsafeAttToK k,) <$> x, r)

splitRoot :: forall k a. k ⊆ Int => IntMap k a -> [IntMap k a]
splitRoot = attenuateIt @k (IM.splitRoot @a)

isSubmapOf :: forall k a. Eq a => IntMap k a -> IntMap k a -> Bool
isSubmapOf = attenuate (IM.isSubmapOf @a)

isSubmapOfBy
  :: forall k a b. (a -> b -> Bool) -> IntMap k a -> IntMap k b -> Bool
isSubmapOfBy = attenuate (IM.isSubmapOfBy @a @b)

isProperSubmapOf :: forall k a. Eq a => IntMap k a -> IntMap k a -> Bool
isProperSubmapOf = attenuate (IM.isProperSubmapOf @a)

isProperSubmapOfBy
  :: forall k a b. (a -> b -> Bool) -> IntMap k a -> IntMap k b -> Bool
isProperSubmapOfBy = attenuate (IM.isProperSubmapOfBy @a @b)

-- $minmax_underlying_ints
--
-- For these functions, "min" and "max" refer to the underlying 'Int'
-- representation, and not to any user-defined 'Ord'ering.  For example, the
-- key found by 'lookupMin' will be the one with the least 'Int' value in the
-- map, but might 'compare' 'GT' some or all other entries, depending on the
-- key type's 'Ord' instance.

lookupMin :: forall k a. k ⊆ Int => IntMap k a -> Maybe (k, a)
lookupMin = attenuateIt @k (IM.lookupMin @a)

lookupMax :: forall k a. k ⊆ Int => IntMap k a -> Maybe (k, a)
lookupMax = attenuateIt @k (IM.lookupMax @a)

findMin :: forall k a. (HasCallStack, k ⊆ Int) => IntMap k a -> (k, a)
findMin = fromMaybe (error "findMin: empty map") . lookupMin

findMax :: forall k a. (HasCallStack, k ⊆ Int) => IntMap k a -> (k, a)
findMax = fromMaybe (error "findMax: empty map") . lookupMax

deleteMin :: forall k a. k ⊆ Int => IntMap k a -> IntMap k a
deleteMin = attenuateIt @k (IM.deleteMin @a)

deleteMax :: forall k a. k ⊆ Int => IntMap k a -> IntMap k a
deleteMax = attenuateIt @k (IM.deleteMax @a)

deleteFindMin
  :: forall k a. (HasCallStack, k ⊆ Int) => IntMap k a -> ((k, a), IntMap k a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map") . minViewWithKey

deleteFindMax
  :: forall k a. (HasCallStack, k ⊆ Int) => IntMap k a -> ((k, a), IntMap k a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map") . maxViewWithKey

updateMin :: forall k a. k ⊆ Int => (a -> Maybe a) -> IntMap k a -> IntMap k a
updateMin = attenuateIt @k (IM.updateMin @a)

updateMax :: forall k a. k ⊆ Int => (a -> Maybe a) -> IntMap k a -> IntMap k a
updateMax = attenuateIt @k (IM.updateMax @a)

updateMinWithKey
  :: forall k a. k ⊆ Int => (k -> a -> Maybe a) -> IntMap k a -> IntMap k a
updateMinWithKey = attenuateIt @k (IM.updateMinWithKey @a)

updateMaxWithKey
  :: forall k a. k ⊆ Int => (k -> a -> Maybe a) -> IntMap k a -> IntMap k a
updateMaxWithKey = attenuateIt @k (IM.updateMaxWithKey @a)

minView :: forall k a. k ⊆ Int => IntMap k a -> Maybe (a, IntMap k a)
minView = attenuateIt @k (IM.minView @a)

maxView :: forall k a. k ⊆ Int => IntMap k a -> Maybe (a, IntMap k a)
maxView = attenuateIt @k (IM.maxView @a)

minViewWithKey :: forall k a. k ⊆ Int => IntMap k a -> Maybe ((k, a), IntMap k a)
minViewWithKey = attenuateIt @k (IM.minViewWithKey @a)

maxViewWithKey :: forall k a. k ⊆ Int => IntMap k a -> Maybe ((k, a), IntMap k a)
maxViewWithKey = attenuateIt @k (IM.maxViewWithKey @a)
