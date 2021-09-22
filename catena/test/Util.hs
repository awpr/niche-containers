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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Util (toListOfConcat, mkBatch) where

import Control.Monad (forM_)
import Data.Sequence (Seq)
import GHC.Exts (IsList(..))

import Control.DeepSeq (NFData, force)
import Data.DList (DList)

import Data.Catena (Catena)

{-# SPECIALIZE NOINLINE mkBatch :: Int -> [Int] #-}
{-# SPECIALIZE NOINLINE mkBatch :: Int -> DList Int #-}
{-# SPECIALIZE NOINLINE mkBatch :: Int -> Catena Int #-}
{-# SPECIALIZE NOINLINE mkBatch :: Int -> Seq Int #-}
mkBatch :: (IsList a, Item a ~ Int) => Int -> a
mkBatch n = fromListN n [0..n-1]

toListOfConcat
  :: forall f a
   . (a ~ f Int, NFData a, IsList a, Monoid a, Item a ~ Int)
  => Int -> a -> a
toListOfConcat n chunk = force $ foldMap (\ !_ -> chunk) ([0..n] :: [Int])
