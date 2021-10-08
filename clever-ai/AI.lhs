{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wall #-}
\begin{code}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import GameStuff (GameState, Turns (..))
import Submission2 (logic, initialState, AIState(turn), Strategy (..))
import System.IO
import Data.Functor.Identity
import Debug.Trace
import System.Environment

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

stateify :: (st -> (a, b, st)) -> State st (a, b)
stateify l
  = StateT $ \st ->
      let (a, b, st') = l st in Identity ((a, b), st')

main :: IO ()
main = do
  [strat] <- map (read @Strategy) <$> getArgs
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  contents <- map (read @GameState) . lines <$> getContents
  flip evalStateT initialState $ forM_ contents $ \ms -> do
    Turns t <- gets turn
    (orders, logs) <- mapStateT (return @IO . runIdentity) (stateify (logic strat ms))
    liftIO $ when (not (null logs)) $ do
      traceIO ("-------------- " ++ show t ++ ": --------------")
      traceIO (unlines logs)
    liftIO (print orders)

\end{code}

