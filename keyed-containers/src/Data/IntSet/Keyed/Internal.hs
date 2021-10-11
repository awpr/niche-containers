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

{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IntSet.Keyed.Internal where

import Prelude hiding ((.))

import Control.Category ((.))
import Control.DeepSeq (NFData)
import qualified Data.IntSet as IS
import Data.Semigroup (Semigroup(stimes))
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts

import Data.Portray (Portray)
import Data.Type.Attenuation
         ( type (⊆), Attenuable, Attenuation
         , attenuate, attenuation, coercible, withAttenuation
         )
import Data.Type.Attenuation.Unsafe (unsafeSym)
import Data.Wrapped (Wrapped(..))

-- | A set type that uses 'IS.IntSet' internally to store newtyped elements.
newtype IntSet a = IntSet
  { getIntSet :: IS.IntSet
    -- ^ Invariant: contains only elements valid for coercion to @a@.
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype NFData

instance a ⊆ Int => Semigroup (IntSet a) where
  (<>) = union
  stimes _ = id

instance a ⊆ Int => Monoid (IntSet a) where
  mempty = empty

deriving via Wrapped IsList (IntSet a)
  instance (Portray a, a ⊆ Int) => Portray (IntSet a)

instance a ⊆ Int => IsList (IntSet a) where
  type Item (IntSet a) = a
  fromList = fromList
  toList = toList

type role IntSet representational

-- | Attenuations between IntSet are fine if keys and values are attenuable.
instance a ⊆ b => Attenuable (IntSet a) (IntSet b) where
  attenuation =
    coercible @IS.IntSet @(IntSet b) . attenuation

instance Attenuable (IntSet a) IS.IntSet where
  -- Note because the underlying IntSet stores Ints regardless of anything
  -- we're capable of doing in this module, we don't need to know a ⊆ Int to
  -- coerce safely to the underlying IntSet: there aren't /actually/ any @a@s
  -- being coerced when doing that, we're just forgetting that we intended to
  -- interpret some Ints as ks.
  attenuation = attenuation . coercible @(IntSet a) @IS.IntSet

-- | Attenuations from raw 'IS.IntSet' are fine for types attenuable /from/ Int.
instance Int ⊆ a => Attenuable IS.IntSet (IntSet a) where
  attenuation = coercible @IS.IntSet @(IntSet a) . attenuation

intSetIsNewtype :: forall a. Attenuation IS.IntSet (IntSet a)
intSetIsNewtype = coercible

empty :: IntSet a
empty = IntSet IS.empty

(\\-) :: (Attenuable a b => r) -> Attenuation a b -> r
r \\- att = withAttenuation att r

union :: forall a. IntSet a -> IntSet a -> IntSet a
union = attenuate IS.union
  \\- intSetIsNewtype @a

fromList :: forall a. a ⊆ Int => [a] -> IntSet a
fromList = attenuate IS.fromList
  \\- intSetIsNewtype @a

toList :: forall a. a ⊆ Int => IntSet a -> [a]
toList = attenuate IS.toList
  \\- unsafeSym (attenuation @a @Int)
