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
import Gauge (bench, bgroup, defaultMain, nf)

import Data.Catena (Catena)
import Util (toListOfConcat, mkBatch)

main :: IO ()
main = do
  defaultMain
    [ bgroup "toListOfConcat" $
        [ bgroup (shows n $ showChar 'x' $ shows m "")
            [ bench "[]" $ nf (toListOfConcat @[] n) (mkBatch m)
            , bench "Catena" $ nf (toListOfConcat @Catena n) (mkBatch m)
            , bench "DList" $ nf (toListOfConcat @DList n) (mkBatch m)
            , bench "Seq" $ nf (toListOfConcat @Seq n) (mkBatch m)
            ]
        | (m, n) <-
            [ (10, 200000)
            , (100, 20000)
            , (1000, 2000)
            , (10000, 200)
            , (10, 20000)
            , (100, 2000)
            , (1000, 200)
            , (10000, 20)
            ]
        ] ++
        [ bgroup (shows n "xOverloadedLists")
            [ bench "[]" $
                nf (toListOfConcat @[] n) [0, 1, 2, 3, 4, 5, 6, 7]
            , bench "Catena" $
                nf (toListOfConcat @Catena n) [0, 1, 2, 3, 4, 5, 6, 7]
            , bench "DList" $
                nf (toListOfConcat @DList n) [0, 1, 2, 3, 4, 5, 6, 7]
            , bench "Seq" $
                nf (toListOfConcat @Seq n) [0, 1, 2, 3, 4, 5, 6, 7]
            ]
        | n <- [10, 1000, 1000000]
        ]

    ]
