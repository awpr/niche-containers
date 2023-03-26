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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Sequence (Seq)
import GHC.Exts (IsList(..))

import Control.DeepSeq (NFData, force)
import Data.DList (DList)
import Weigh (func', mainWith, wgroup, setColumns, Column(..))

import Data.Catena (Catena)
import Util (mkBatch, toListOfConcat)

main :: IO ()
main = do
  mainWith $ do
    setColumns [Case, Allocated, GCs, Max, MaxOS, Live]
    wgroup "toListOfConcat" $ do
      forM_ @[] [(10, 20000), (100, 2000), (1000, 200), (10000, 20)] $
        \ (n, m) ->
        wgroup (shows n $ showChar 'x' $ shows m "") $ do
          func' "[]" (toListOfConcat @[] n) (mkBatch m)
          func' "Catena" (toListOfConcat @Catena n) (mkBatch m)
          func' "DList" (toListOfConcat @DList n) (mkBatch m)
          func' "Seq" (toListOfConcat @Seq n) (mkBatch m)
      forM_ @[] [10, 1000, 1000000] $ \n ->
        wgroup (shows n "xOverloadedLists") $ do
          func' "[]" (toListOfConcat @[] n) [0, 1, 2, 3, 4, 5, 6, 7]
          func' "Catena" (toListOfConcat @Catena n) [0, 1, 2, 3, 4, 5, 6, 7]
          func' "DList" (toListOfConcat @DList n) [0, 1, 2, 3, 4, 5, 6, 7]
          func' "Seq" (toListOfConcat @Seq n) [0, 1, 2, 3, 4, 5, 6, 7]
