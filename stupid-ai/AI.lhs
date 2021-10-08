\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import GameStuff
import System.IO
import Data.Functor.Identity
import Debug.Trace

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

data AIState = AIState
  { turn :: Turns
  }

initialState :: AIState
initialState = AIState
  { turn = 0
  }

type Log = [String]

logic :: GameState -> State AIState ([Order], Log)
logic st = do
  AIState current_turn <- get
  put (AIState (current_turn + 1))
  return (concatMap attackAllNeighbours (availableTargets st), dummyLog st)

dummyLog :: GameState -> Log
dummyLog st = ["We have " ++ show (length (ourPlanets st)) ++ " planets!"]

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _)
  = M.filter ourPlanet ps

availableTargets :: GameState -> [(Planet, Wormholes)]
availableTargets st
  = map (\(pId, p) -> (p, wormholesFrom (Source pId) st)) (M.assocs (ourPlanets st))

attackAllNeighbours :: (Planet, Wormholes) -> [Order]
attackAllNeighbours (Planet _ (Ships s) _, ws)
  | null ws = []
  | otherwise
      = let each = max 0 (s `div` length ws)
        in if each > 0 then map (\(wId, _) -> Order wId (Ships each)) (M.assocs ws) else []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  contents <- map (read @GameState) . lines <$> getContents
  flip evalStateT initialState $ forM_ contents $ \ms -> do
    Turns t <- gets turn
    (orders, logs) <- mapStateT (return @IO . runIdentity) (logic ms)
    liftIO $ when (not (null logs)) $ do
      traceIO ("-------------- " ++ show t ++ ": --------------")
      traceIO (unlines logs)
    liftIO (print orders)

\end{code}

