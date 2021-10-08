
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}

module Submission2 where

import GameStuff hiding (example1, example2, example3, lookupPlanet)
import qualified Data.Map as M
import Data.List (unfoldr)
import Data.List
import Data.Maybe
import Text.Printf


import Prelude hiding (maximum)
import Data.Maybe (fromJust)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Array
import Data.List (nub, sortBy, maximumBy, minimumBy, tails, inits, mapAccumL, (\\))
import Data.Map (Map)
import qualified Data.Map as M


deriving instance (Integral Growth)
deriving instance (Enum Growth)
deriving instance (Real Growth)

data Strategy
  = Pacifist
  | ZergRush
  | PlanetRankRush
  | Skynet
  deriving (Enum, Bounded, Show, Read)

logic :: Strategy -> GameState -> AIState -> ([Order], Log, AIState)
logic strat gs ai
  = let logic' = case strat of
          Pacifist       -> pacifist
          ZergRush       -> zergRush
          PlanetRankRush -> planetRankRush
          Skynet         -> skynet
    in logic' gs ai {turn = turn ai + 1}

data AIState = AIState
  { turn :: Turns
  , rushTarget :: Maybe PlanetId
  , sortedTargets :: [(PlanetId,PlanetRank)]
  }

initialState :: AIState
initialState = AIState
  { turn = 0
  , rushTarget = Nothing
  , sortedTargets = []
  }

type Log = [String]

pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist _ ai = ([], ["Do no harm."], ai)

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet (GameState ps _ _)
  | length enemyPlanets == 0 = Nothing
  | otherwise          = Just ((map fst enemyPlanets) !! 0)
  where
    enemyPlanets = filter (\(a,b) -> enemyPlanet b) (M.toList ps)


send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId mShips st
  | not (ourPlanet planet) = []
  | (mShips == Nothing || fromJust mShips > totalShips) = [(Order wId totalShips)]
  | (fromJust mShips <= totalShips) = [(Order wId (fromJust mShips))]
  | otherwise        = []
 where
  Wormhole (Source src) _ _ = lookupWormhole wId st
  planet@(Planet _ totalShips _) = lookupPlanet src st

shortestPath :: PlanetId -> PlanetId -> GameState
             -> Maybe (Path (WormholeId, Wormhole))
shortestPath src dst st
  = case filter ((== dst) . target) (shortestPaths st src) of
      [] -> Nothing
      (x : _) -> Just x

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _) = M.filter ourPlanet ps

lookupWormhole :: WormholeId -> GameState -> Wormhole
lookupWormhole wId (GameState _ wormholes _)
  = wormholes M.! wId

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState planets _ _)
  = planets M.! pId

attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId gs = [y | x <- map fst (M.toList ownedPlanets), y <- getOrder x]
  where
    ownedPlanets = ourPlanets gs
    wIdFromPath (Path w edges) = fst (last edges)
    getOrder src
      | isNothing (shortestPath src targetId gs)  = []
      | otherwise = send (wIdFromPath (fromJust (shortestPath src targetId gs))) Nothing gs

zergRush :: GameState -> AIState
         -> ([Order], Log, AIState)
zergRush gs ai@(AIState t rT _)
  | not (rT == Nothing) && not (ourPlanet (lookupPlanet (fromJust rT) gs))
    =(attackFromAll (fromJust rT) gs, [] , (AIState (t+1) rT []))
  | otherwise = ([],[],(AIState t target []))
      where
        target = (findEnemyPlanet gs)

newtype PageRank = PageRank Double
  deriving (Num, Eq, Ord, Fractional)

type PageRanks pageId = Map pageId PageRank

instance Show PageRank where
  show (PageRank p) = printf "%.4f" p

initPageRanks :: (Graph g e pageId, Ord pageId)
              => g -> PageRanks pageId
initPageRanks g = M.fromList [ (p, PageRank (1 / fromIntegral n))
                             | p <- ps ]
  where ps = vertices g
        n  = length ps

example1 :: [(String, String, Integer)]
example1 = [("a","b",1), ("a","c",1), ("a","d",1),
            ("b","a",1), ("c","a",1), ("d","a",1), ("c","d",1)]

initPageRank' :: Map pageId a -> PageRanks pageId
initPageRank' mp = M.map (\x ->  (1 / fromIntegral n)) mp
  where
    n = M.size mp

nextPageRank :: (Ord pageId, Edge e pageId, Graph g e pageId) =>
  g -> PageRanks pageId -> pageId -> PageRank
nextPageRank g pr i = (1 - d) / n + d * sum [ pr M.! j / t j
                                            | j <- s i ]
 where
  d   = 0.85
  n   = fromIntegral (length (vertices g))
  t j = fromIntegral (length (edgesFrom g j))
  s i = map source (edgesTo g i)

nextPageRanks :: Ord pageId => Graph g e pageId =>
  g -> PageRanks pageId -> PageRanks pageId
nextPageRanks g pr = M.mapWithKey (const . nextPageRank g pr) pr

pageRanks :: (Ord pageId, Graph g e pageId) => g -> [PageRanks pageId]
pageRanks g = iterate (nextPageRanks g) (initPageRanks g)

pageRank :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank g = pageRanks g !! 200

nextPageRank' :: (Ord pageId, Edge e pageId, Graph g e pageId) =>
  g -> PageRanks pageId -> PageRank -> pageId -> PageRank -> Maybe PageRank
nextPageRank' g pr k i pri
  | abs (pri - pri') < k  = Nothing
  | otherwise             = Just pri'
 where
   pri' = nextPageRank g pr i

nextPageRanks' :: Ord pageId => Graph g e pageId =>
  g -> PageRank -> PageRanks pageId -> Maybe (PageRanks pageId)
nextPageRanks' g k pr = case M.mapAccumWithKey nextPageRank'' True pr of
                           (True,  pr)  -> Nothing
                           (False, pr') -> Just pr'
  where
    nextPageRank'' converged i pri = case nextPageRank' g pr k i pri of
                            Nothing   -> (converged, pri)
                            Just pri' -> (False, pri')

pageRanks' :: (Ord pageId, Graph g e pageId)
  => g -> PageRank -> [PageRanks pageId]
pageRanks' g k = iterateMaybe (nextPageRanks' g k) (initPageRanks g)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pageRank' :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank' g
  | (length maps) > 200 = maps !! 200
  | otherwise           = last maps
  where
    maps = pageRanks' g 0.0001

example2 :: GameState
example2 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 7))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 2))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 3))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 6))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole (Source 0) (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole (Source 0) (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole (Source 0) (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole (Source 1) (Target 0) (Turns 1))
    , (WormholeId 4, Wormhole (Source 2) (Target 0) (Turns 1))
    , (WormholeId 5, Wormhole (Source 3) (Target 0) (Turns 1))
    , (WormholeId 6, Wormhole (Source 2) (Target 3) (Turns 1))
    ]
  fleets = []

newtype PlanetRank = PlanetRank Double
  deriving (Num, Eq, Ord, Fractional, Floating)

type PlanetRanks = Map PlanetId PlanetRank

instance Show PlanetRank where
  show (PlanetRank p) = printf "%.4f" p

initPlanetRanks :: GameState -> PlanetRanks
initPlanetRanks g = M.fromList [ (p, PlanetRank (1 / fromIntegral n))
                               | p <- ps ]
  where ps = vertices g
        n  = length ps

planetRank :: GameState -> PlanetRanks
planetRank g = planetRanks g !! 200

planetRanks :: GameState -> [PlanetRanks]
planetRanks g = iterate (nextPlanetRanks g) (initPlanetRanks g)

nextPlanetRanks :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks g pr = M.mapWithKey (const . nextPlanetRank g pr) pr

nextPlanetRank :: GameState -> PlanetRanks
               -> PlanetId -> PlanetRank
nextPlanetRank g@(GameState planets ws _) pr i =
 (1 - d) / n + d * sum [ pr M.! j * growth i / growths j
                       | j <- targets i ]
 where
  d   = 0.85
  n   = fromIntegral (length planets)

  growth :: PlanetId -> PlanetRank
  growth i  = (\(Planet _ _ g) -> fromIntegral g)
                                  (planets M.! i)
  targets :: PlanetId -> [PlanetId]
  targets i = [target wh | (_, wh) <- M.toList ws, source wh == i]

  growths :: PlanetId -> PlanetRank
  growths j = sum [growth (source wh) | (_,wh) <- M.toList ws , target wh == j]

checkPlanetRanks :: PlanetRanks -> PlanetRank
checkPlanetRanks = sum . M.elems

sortPlanets :: [(PlanetId, PlanetRank)] -> [(PlanetId,PlanetRank)]
sortPlanets ranks = reverse (sortBy (compare `on` snd) ranks)

findNextPlanet :: GameState -> [(PlanetId,PlanetRank)] -> PlanetId
findNextPlanet gs ps
  | lst == [] = 0
  | otherwise = fst (lst !! 0)
    where
      lst = filter (\(a,b) -> enemyPlanet (lookupPlanet a gs)) ps

planetRankRush :: GameState -> AIState
               -> ([Order], Log, AIState)
planetRankRush gs ai@(AIState t rT ps)
  | ps == [] = ([], [], (AIState t rT (sortPlanets (M.toList (planetRank gs)))))
  | rT == Nothing || ourPlanet (lookupPlanet (fromJust rT) gs)
    = ([],[],(AIState t (Just (findNextPlanet gs ps)) ps))
  | otherwise = (attackFromAll (fromJust rT) gs, [], (AIState (t+1) rT ps))
    where
      newTarget = findNewTarget (filter (\(a,b) -> not (ourPlanet (lookupPlanet a gs))) (M.toList (planetRank gs)))
      findNewTarget [] = Just 0
      findNewTarget ps = Just (fst (foldl1 (\x y -> if snd x > snd y then x else y) ps))




-- ============================================= SKYNET ======================================================
{-

nextPlanetRank' :: GameState -> PlanetRanks
               -> PlanetId -> PlanetRank
nextPlanetRank' g@(GameState planets ws _) pr i =
 (1 - d) / n + d * sum [ (pr M.! (target j) /  fromIntegral (weight j) ) * growth i / growths (target j)
                       | j <- edgesFrom g i]
 where
  d   = 0.85
  n   = fromIntegral (length planets)

  growth :: PlanetId -> PlanetRank
  growth i  = ((\(Planet _ _ g) -> fromIntegral g)
                                  (planets M.! i))
  targets :: PlanetId -> [PlanetId]
  targets i = [target wh | (_, wh) <- M.toList ws, source wh == i]

  growths :: PlanetId -> PlanetRank
  growths j = sum [growth (source wh) | (_,wh) <- M.toList ws , target wh == j]

planetRank' :: GameState -> PlanetRanks
planetRank' g = planetRanks' g !! 200

planetRanks' :: GameState -> [PlanetRanks]
planetRanks' g = iterate (nextPlanetRanks' g) (initPlanetRanks g)

nextPlanetRanks' :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks' g pr = M.mapWithKey (const . nextPlanetRank' g pr) pr

-}
tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate (u,v) f = array (u,v) [ (i, f i) | i <- range (u, v)]

maxFirstElem :: (Num a, Ord a) => [(a, [b])] -> (a, [b])
maxFirstElem  = foldl (\a b -> if fst a >= fst b then a else b) (0, [])

bknapsack :: [(PlanetId, Int, PlanetRank)] -> Int -> (PlanetRank, [PlanetId])
bknapsack wvs c= table ! (c,0)
  where
    table :: Array (Int,Int) (PlanetRank, [PlanetId])
    table = tabulate ((0,0), (c, length wvs)) mbknapsack

    mbknapsack :: (Int,Int) -> (PlanetRank, [PlanetId])
    mbknapsack (0 , _)  = (0, [])
    mbknapsack (weight, i)
      | i > size                                                  = (0, [])
      | weight < minimum (map snd3 [wvs !! is | is <- [i..size]]) = (0, [])
      | (w + 1) > weight = table ! (weight,(i + 1))
      | otherwise  = maxFirstElem [table ! (weight,(i + 1)),
                                    (value + v, n : names)]
          where
            (n, w, v) = wvs !! i
            (value, names) = table ! ((weight - w - 1), (i + 1))
            size = (length wvs) - 1
            snd3 (a, b, c) = b
--weight is capacity
aggressive = 1.0

skynet :: GameState -> AIState -> ([Order],Log,AIState)
skynet gs ai@(AIState t rT ps)
  | ps == [] = ([], [], (AIState t rT  (M.toList (planetRank gs))))
  | otherwise =  ([x|p<- allPlanets, x<-(orderSinglePlanet gs ps p)], [], ai)
    where
      allPlanets = M.toList (ourPlanets gs)
      orderSinglePlanet :: GameState -> [(PlanetId, PlanetRank)] -> (PlanetId,Planet) -> [Order]
      orderSinglePlanet gs ps planet
        = expansion_order
          where
            (_, planets) = bknapsack values (round ( fromIntegral (getShips (snd planet)) *aggressive))
            adj = map (\(a,b) ->(target b, weight b)) (M.toList (wormholesFrom (Source (fst planet)) gs))
            values = [( x , round (fromIntegral (getShips (lookupPlanet x gs)) * (1.0 + 0.05 * (fromIntegral y) )),(fromJust (lookup x ps)) * (if (enemyPlanet (lookupPlanet x gs)) then 5.0 else (if (ourPlanet (lookupPlanet x gs)) then 1.0 else 1.0)  ) / ((fromIntegral y)**0.3) ) | (x,y) <- adj]
            expansion_order =  [(Order (getWormhole (fst planet) pl gs) (Ships ((getShips (lookupPlanet pl gs) + 1))))  | pl <- planets]
            ships_for_expansion = getShips (snd planet) - usedships expansion_order


usedships :: [Order] -> Int
usedships [] = 0
usedships ((Order _ (Ships i)):xs) = i + usedships xs


getShips (Planet _ (Ships s) _) = s

getWormhole :: PlanetId -> PlanetId -> GameState -> WormholeId
getWormhole s d gs = (intersect ( map fst (M.toList (wormholesFrom (Source s) gs))) (map fst (M.toList (wormholesTo (Target d) gs)))) !! 0

