{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AutoTest where

import Submission2

import GameStuff
import Data.Map (fromList)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Coerce
import Control.Exception
import GHC.Generics
import Data.Type.Bool
import Data.List
import Data.Function

import Control.Monad
import Control.DeepSeq

import System.IO

deriving instance Num Target
deriving instance Num Source
deriving instance Num WormholeId
deriving instance Eq Order
deriving instance Ord Order

deriving instance Generic AIState

deriving instance Generic PlanetRank
deriving instance Generic PageRank
deriving instance Generic PlanetId
deriving instance Generic Order
deriving instance Generic WormholeId
deriving instance Generic Ships

instance NFData PageRank
instance NFData PlanetRank
instance NFData PlanetId
instance NFData Order
instance NFData WormholeId
instance NFData Ships

instance Read PageRank where
  readsPrec = coerce (readsPrec @Double)

instance Read PlanetRank where
  readsPrec = coerce (readsPrec @Double)

main :: IO ()
main = do
  pageRankTests_      <- readFile "pageRankTests.txt"
  planetRankTests_    <- readFile "planetRankTests.txt"
  findEnemyTests_     <- readFile "findEnemyTests.txt"
  attackFromAllTests_ <- readFile "attackFromAllTests.txt"
  sendTests_          <- readFile "sendTests.txt"

  let forceTests t = (let !_ = force (map snd t) in return t)
        `catch` (\(e :: SomeException) -> return [])

  let planetRankTests :: [(GameState, PlanetRanks)]
      planetRankTests = read planetRankTests_

      pageRankTests :: [(GameState, PageRanks PlanetId)]
      pageRankTests = read pageRankTests_

      findEnemyTests :: [(GameState, Maybe PlanetId)]
      findEnemyTests = read findEnemyTests_

      sendTests :: [(GameState, WormholeId, Maybe Ships, [Order])]
      sendTests = read sendTests_

      attackFromAllTests :: [(GameState, PlanetId, [Order])]
      attackFromAllTests = read attackFromAllTests_

  let planetRankResults :: [(GameState, PlanetRanks)]
      planetRankResults = map (\(gs, _) -> (gs, planetRank gs)) planetRankTests

  planetRankResults' <- forceTests planetRankResults

  let pageRankResults :: [(GameState, PageRanks PlanetId)]
      pageRankResults = map (\(gs, _) -> (gs, pageRank' gs)) pageRankTests

  pageRankResults' <- forceTests pageRankResults

  let findEnemyResults
        = map (\(gs, _) -> (gs, findEnemyPlanet gs)) findEnemyTests

  findEnemyResults' <- forceTests findEnemyResults

  let attackFromAllResults
        = map (\(gs, pId, _) -> (gs, (pId, attackFromAll pId gs))) attackFromAllTests

  attackFromAllResults' <- forceTests attackFromAllResults

  let sendResults
        = map (\(gs, wId, m_ships, _) -> (gs, (wId, m_ships, send wId m_ships gs))) sendTests

  sendResults' <- forceTests sendResults


  let pls_results = zipWith (same `on` snd) planetRankTests planetRankResults'
      pr_results  = zipWith (same `on` snd) pageRankTests pageRankResults'

      pr_score = length (filter id pr_results)
      pr_possible = length pageRankTests

      plr_score = length (filter id pls_results)
      plr_possible = length planetRankTests

      fe_results = zipWith ((==) `on` snd) findEnemyTests findEnemyResults'
      fe_score = length (filter id fe_results)
      fe_possible = length findEnemyTests

      afa_results = zipWith (\(gs, _, os) (_, (_, os')) -> sameOrders os os') attackFromAllTests attackFromAllResults'
      afa_score = length (filter id afa_results)
      afa_possible = length attackFromAllTests

      s_results = zipWith (\(gs, _, _, os) (_, (_, _, os')) -> os == os') sendTests sendResults'
      s_score = length (filter id s_results)
      s_possible = length sendTests

      rt_score = fromEnum hasRushTargetField

      results
       = [ ("Find enemy Tests", fe_score, fe_possible)
         , ("AIState rushTarget Test", rt_score, 1)
         , ("Send Tests", s_score, s_possible)
         , ("Attack from all Tests", afa_score, afa_possible)
         , ("PageRank Tests", pr_score, pr_possible)
         , ("PlanetRank Tests", plr_score, plr_possible)
         ]

  putStrLn (prettyTestResult results)

epsilon :: Fractional a => a
epsilon = 0.001

same :: (Ord k, Fractional a, Ord a) => M.Map k a -> M.Map k a -> Bool
same ps1 ps2 = and (M.map (< epsilon) (M.unionWith (\p1 p2 -> abs (p1 - p2)) ps1 ps2))

sameOrders :: [Order] -> [Order] -> Bool
sameOrders os1 os2
  = normalise os1 == normalise os2
  where normalise :: [Order] -> [Order]
        normalise os = map (foldr1 combine) (groupBy ((==) `on` wh) (sortOn wh os))
        combine :: Order -> Order -> Order
        combine (Order w s) (Order _ s') = Order w (s + s')
        wh (Order w _) = w

prettyTestResult :: [(String, Int, Int)] -> String
prettyTestResult results = "[" ++ intercalate ", " (map showResult results) ++ "]"
  where
    showKV k v = show k ++ ": " ++ show v
    showResult (name, score, possible)
      = concat [ "{"
               , intercalate ", " [ showKV "name" name
                                  , showKV "score" score
                                  , showKV "possible" possible
                                  ]
               , "}"
               ]

type family HasRushTargetField (s :: * -> *) :: Bool where
  HasRushTargetField (D1 _ x) = HasRushTargetField x
  HasRushTargetField (C1 _ x) = HasRushTargetField x
  HasRushTargetField (l :*: r) = HasRushTargetField l || HasRushTargetField r
  HasRushTargetField (l :+: r) = HasRushTargetField l && HasRushTargetField r
  HasRushTargetField (S1 ('MetaSel ('Just "rushTarget") _ _ _) (Rec0 (Maybe PlanetId))) = 'True
  HasRushTargetField _ = 'False

class KnownBool (b :: Bool) where
  boolVal :: Bool

instance KnownBool 'True where
  boolVal = True

instance KnownBool 'False where
  boolVal = False

hasRushTargetField :: Bool
hasRushTargetField = boolVal @(HasRushTargetField (Rep AIState))
