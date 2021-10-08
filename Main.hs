{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#ifndef BUILD_DIR
#define BUILD_DIR "."
#endif

#ifndef OUT_DIR
#define OUT_DIR "."
#endif

module Main (main) where

import           Control.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Data.List (sortOn, partition, intercalate)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust, fromMaybe)
import           Data.IORef
import           System.Environment
import           System.IO
import           System.Process
import           System.Random
import           System.Exit
import           Data.Semigroup
import           System.Timeout
import           Data.Time.Clock

import           Map


log1_file :: String
log1_file = OUT_DIR <> "/log1.txt"

log2_file :: String
log2_file = OUT_DIR <> "/log2.txt"

main :: IO ()
main = do
  let ?radius = 25

  Arguments
    { arg_p1
    , arg_p2
    , arg_seed
    , arg_strategy1
    , arg_strategy2
    , arg_ui
    , arg_turns
    , arg_recomp
    , arg_dump_map
    } <- readArguments

  seed <- case arg_seed of
    Nothing   -> getStdGen
    Just seed' -> return seed'

  let strategy1 = case arg_strategy1 of
        Nothing -> Skynet
        Just s -> s

      strategy2 = case arg_strategy2 of
        Nothing -> Skynet
        Just s -> s

  setStdGen seed
  let ?seed = seed
  p1 <- compileClient arg_recomp arg_p1 Player1
  p2 <- compileClient arg_recomp arg_p2 Player2

  (positions, planets, wormholes) <- randomMap seed

  when arg_dump_map $ do
    print (GameState planets wormholes [])
    exitSuccess

  let total_time_per_player = 60
  timer_1 <- newIORef @NominalDiffTime total_time_per_player
  timer_2 <- newIORef @NominalDiffTime total_time_per_player


  let p1config = (proc p1 [show strategy1])
                   {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
      p2config = (proc p2 [show strategy2])
                   {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

  let serverState
        = initialServerState
            arg_turns
            timer_1
            timer_2
            positions
            (GameState planets wormholes [])

  when (arg_ui == Headless) $ do
    putStrLn "Running in headless mode..."
    putStrLn ("Seed: " <> showSeed ?seed)

  withCreateProcess p1config $ \(Just p1_in) (Just p1_out) (Just p1_err) p1h ->
    withCreateProcess p2config $ \(Just p2_in) (Just p2_out) (Just p2_err) p2h -> do

      when (arg_ui == TUI) $ do
        putStr "\ESC[2J" -- clear background
        liftIO $ drawBG

      writeFile log1_file ""
      writeFile log2_file ""

      let channel1 = makeChannel Player1 timer_1 p1_in p1_out p1_err
          channel2 = makeChannel Player2 timer_2 p2_in p2_out p2_err

      result <- evalStateT (loop arg_ui channel1 channel2) serverState
        `catch` (\(e :: AsyncException) -> case e of
            UserInterrupt -> return Interrupted
            e' -> throw e'
        ) `catch` (\(e :: SomeException) -> do
            when (arg_ui == TUI) $ do
              putStr "\ESC[2J"
              resetCol

            exitCode1 <- getProcessExitCode p1h
            exitCode2 <- getProcessExitCode p2h

            putStrLn "Exception:"
            putStrLn (show e)
            case exitCode1 of
              Just (ExitFailure _) -> do
                putStrLn "Player1 crashed: "
                r1 <- hReady p1_err
                if r1 then
                  hGetContents p1_err >>= putStrLn
                  else return ()
              _ -> return ()

            case exitCode2 of
              Just (ExitFailure _) -> do
                putStrLn "Player2 crashed: "
                r2 <- hReady p2_err
                if r2 then
                  hGetContents p2_err >>= putStrLn
                  else return ()
              _ -> return ()
            terminateProcess p1h
            terminateProcess p2h
            exitFailure
          )

      when (arg_ui == TUI) $ do
        putStrLn ""
        resetCol
      case result of
        Player1Won  -> putStrLn "Player 1 won"
        Player2Won  -> putStrLn "Player 2 won"
        Draw        -> putStrLn "Draw"
        Interrupted -> putStrLn "Game interrupted"

      return ()

  return ()

data Strategy
  = Pacifist
  | ZergRush
  | PlanetRankRush
  | Skynet
  deriving (Enum, Bounded, Show, Read)

data UI = Headless | TUI
  deriving (Ord, Eq)

data Arguments = Arguments
  { arg_p1        :: String
  , arg_p2        :: String
  , arg_seed      :: Maybe StdGen
  , arg_strategy1 :: Maybe Strategy
  , arg_strategy2 :: Maybe Strategy
  , arg_ui        :: UI
  , arg_turns     :: Turns
  , arg_recomp    :: Bool
  , arg_dump_map  :: Bool
  }

defaultArguments :: String -> String -> Arguments
defaultArguments arg_p1 arg_p2
  = Arguments { arg_p1
              , arg_p2
              , arg_seed=Nothing
              , arg_strategy1=Nothing
              , arg_strategy2=Nothing
              , arg_ui=TUI
              , arg_turns=1000
              , arg_recomp=True
              , arg_dump_map=False
              }

readArguments :: IO Arguments
readArguments = do
  progName <- getProgName
  let all_strats :: [Strategy]
      all_strats = [minBound..maxBound]

      usage
        = putStrLn $ concat
          [ "Usage: ", progName, " <bot1> <bot2> "
          , "[--seed seed] "
          , "[--strategy1 [ " , intercalate " | " (map show all_strats) , " ]] "
          , "[--strategy2 [ " , intercalate " | " (map show all_strats) , " ]] "
          , "[--headless] "
          , "[--dump-map]"
          , "[--turns turns] "
          ]

      flag ('-' : '-' : _) = True
      flag _ = False

      go args ("--seed" : seed : rest)
        | not (flag seed)
        = go args {arg_seed = Just (readSeed seed)} rest
      go args ("--strategy1" : strategy : rest)
        | not (flag strategy)
        , [(strat, "")] <- reads @Strategy strategy
        = go args {arg_strategy1 = Just strat} rest
      go args ("--strategy2" : strategy : rest)
        | not (flag strategy)
        , [(strat, "")] <- reads @Strategy strategy
        = go args {arg_strategy2 = Just strat} rest
      go args ("--headless" : rest)
        = go args {arg_ui = Headless} rest
      go args ("--turns" : ts : rest)
        | not (flag ts)
        = go args {arg_turns = read ts} rest
      go args ("--no-recomp" : rest)
        = go args {arg_recomp = False} rest
      go args ("--dump-map" : rest)
        = go args {arg_dump_map = True} rest
      go args []
        = return args
      go _ _
        = usage >> exitSuccess


  args <- getArgs
  case args of
    (p1 : p2 : rest) -> go (defaultArguments p1 p2) rest
    _ -> usage >> exitSuccess

compileClient :: Bool -> String -> Player -> IO String
compileClient recomp src_dir p = do
  let binary_name = BUILD_DIR <> "/" <> show p
  let fforce_recomp = if recomp then ["-fforce-recomp"] else []
  callProcess "ghc" $ ["-i" <> src_dir
                      , src_dir <> "/AI.lhs"
                      -- , "-package-env -"
                      , "-outputdir " <> BUILD_DIR <> "/" <> src_dir
                      , "-o", binary_name]
                      ++ fforce_recomp
  return binary_name

data ServerState = ServerState
  { remainingTurns   :: Turns
  , timer1           :: IORef NominalDiffTime
  , timer2           :: IORef NominalDiffTime
  , gameState        :: GameState
  , planet_positions :: Map PlanetId Point
  }

initialServerState
  :: Turns
  -> IORef NominalDiffTime
  -> IORef NominalDiffTime
  -> Map PlanetId Point
  -> GameState
  -> ServerState
initialServerState ts t1 t2 pos gs = ServerState
  { remainingTurns   = ts
  , timer1           = t1
  , timer2           = t2
  , gameState        = gs
  , planet_positions = pos
  }

type Game a = StateT ServerState IO a

data PlayerTimedOutException
  = PlayerTimedOutException Player
  deriving Show

instance Exception PlayerTimedOutException

data Channel = Channel
  { sendGameState :: GameState -> IO [Order]
  , getErrors     :: IO String
  }

makeChannel
  :: Player
  -> IORef NominalDiffTime
  -> Handle
  -> Handle
  -> Handle
  -> Channel
makeChannel player timer_ref stdin_ stdout_ stderr_
  = Channel {sendGameState, getErrors}
  where
    sendGameState :: GameState -> IO [Order]
    sendGameState = \mst -> do
      hPutStrLn stdin_ (show mst)
      hFlush stdin_
      start <- getCurrentTime
      m_orders <- timeout (60 * 10^(6::Integer)) (read @[Order] <$> hGetLine stdout_)
      end <- getCurrentTime

      timer <- readIORef timer_ref

      let new_timer = timer - (diffUTCTime end start)

      when (new_timer < 0) $
        throw (PlayerTimedOutException player)

      writeIORef timer_ref new_timer
      return (fromMaybe [] m_orders)

    getErrors :: IO String
    getErrors = go []
      where go acc = do
              c <- hReady stderr_
              if c then do
                l <- hGetLine stderr_
                go (l : acc)
              else return (unlines (reverse acc))
--------------------------------------------------------------------------------
-- * Utilities

showSeed :: StdGen -> String
showSeed stdgen = map go (show stdgen)
  where go ' ' = '-'
        go x = x

readSeed :: String -> StdGen
readSeed str = read (map go str)
  where go '-' = ' '
        go x = x

lookupPlanet :: PlanetId -> ServerState -> Planet
lookupPlanet pId ServerState{gameState=GameState planets _ _}
  = fromJust (M.lookup pId planets)

updatePlanet :: PlanetId -> (Planet -> Planet) -> ServerState -> ServerState
updatePlanet pId f st
  = st { gameState = updatePlanets (M.update (Just . f) pId) (gameState st)}
  where updatePlanets w (GameState planets wormholes fleets)
          = GameState (w planets) wormholes fleets

lookupWormhole :: WormholeId -> ServerState -> Wormhole
lookupWormhole wId ServerState{gameState=GameState _ wormholes _}
  = fromJust (M.lookup wId wormholes)

getPlanets :: Game Planets
getPlanets = do
  (GameState planets _ _) <- gets gameState
  return planets

getFleets :: Game Fleets
getFleets = do
  (GameState _ _ fleets) <- gets gameState
  return fleets

getTurns :: Game Turns
getTurns =
  gets remainingTurns

playerPlanets :: Player -> Game Planets
playerPlanets player
  = M.filter (\(Planet o _ _) -> o == Owned player) <$> getPlanets

playerTotalFleets :: Player -> Game Fleets
playerTotalFleets player
  = filter (\(Fleet p _ _ _) -> p == player) <$> getFleets

playerTotalGrowth :: Player -> Game Growth
playerTotalGrowth player
  = sum . (map (\(Planet _ _ g) -> g)) . M.elems <$> playerPlanets player

playerTotalShips :: Player -> Game Ships
playerTotalShips player = do
  planets <- playerPlanets player
  fleets  <- playerTotalFleets player
  return
    $ sum (map (\(Planet _ s _) -> s) (M.elems planets))
    + sum (map (\(Fleet _ s _ _) -> s) fleets)

--------------------------------------------------------------------------------
-- * Game logic

loop :: (?radius :: Int, ?seed :: StdGen) => UI -> Channel -> Channel -> Game Result
loop ui ch1 ch2 = do
  st <- gets gameState

  (o1, o2) <- liftIO $ do
    o1 <- sendGameState ch1 st
    o2 <- sendGameState ch2 (invertPlayers st)
    e1 <- getErrors ch1
    e2 <- getErrors ch2
    appendFile log1_file e1
    appendFile log2_file e2
    return (o1, o2)

  departure Player1 o1
  departure Player2 o2
  advancement
  arrival

  when (ui == TUI) $ do
    drawGameState
    liftIO $ threadDelay 50000

  updateServerState

  m_result <- checkEndGame
  case m_result of
    Just result -> return result
    Nothing -> loop ui ch1 ch2

departure :: Player -> [Order] -> Game ()
departure = mapM_ . execute

execute :: Player -> Order -> Game ()
execute p (Order wId ships) = do
  Wormhole (Source sId) _ turns <- gets (lookupWormhole wId)
  Planet owner sourceShips _    <- gets (lookupPlanet sId)
  if (sourceShips >= ships && owner == Owned p)
    then do
    modify (updatePlanet sId (\(Planet _o s _g) -> Planet _o (s - ships) _g))
    modifyGameState $ \(GameState _ps _ws fleets) ->
      GameState _ps _ws (Fleet p ships wId turns : fleets)
    else do
    error "execute: not enough ships!"

growPlanet :: Planet -> Planet
growPlanet p@(Planet Neutral _ _)
  = p
growPlanet (Planet o@(Owned _) ships g@(Growth growth))
  = Planet o (ships + Ships growth) g

advanceFleet :: Fleet -> Fleet
advanceFleet (Fleet _p _s _w (Turns t))
  = Fleet _p _s _w (Turns (t - 1))

advancement :: Game ()
advancement
  = modifyGameState $ \(GameState planets _w fleets) ->
    GameState (M.map growPlanet planets) _w (map advanceFleet fleets)

data PlanetForces = PlanetForces
  { _player1 :: Ships
  , _player2 :: Ships
  , _neutral :: Ships
  }

instance Semigroup PlanetForces where
  PlanetForces p1 p2 n <> PlanetForces p1' p2' n'
    = PlanetForces (p1 + p1') (p2 + p2') (n + n')

instance Monoid PlanetForces where
  mempty = PlanetForces 0 0 0
  mappend = (<>)

existingForces :: Planet -> PlanetForces
existingForces (Planet owner ships _) = case owner of
  Neutral       -> PlanetForces 0     0     ships
  Owned Player1 -> PlanetForces ships 0     0
  Owned Player2 -> PlanetForces 0     ships 0

fleetForce :: Player -> Ships -> PlanetForces
fleetForce Player1 ships = PlanetForces ships 0 0
fleetForce Player2 ships = PlanetForces 0 ships 0

invertPlayers :: GameState -> GameState
invertPlayers (GameState planets wormholes fleets)
  = GameState (invertPlanets planets) (invertWormholes wormholes) (invertFleets fleets)
  where inv Player1 = Player2
        inv Player2 = Player1
        invertOwner (Owned o) = Owned (inv o)
        invertOwner Neutral   = Neutral
        invertPlanet :: Planet -> Planet
        invertPlanet (Planet o s g) = Planet (invertOwner o) s g
        invertPlanets = M.map invertPlanet
        invertWormholes = id
        invertFleet :: Fleet -> Fleet
        invertFleet (Fleet p s w t) = Fleet (inv p) s w t
        invertFleets = map invertFleet

settle :: PlanetForces -> Planet -> Planet
settle (PlanetForces p1 p2 n) (Planet owner _ growth)
  = case sortOn fst [(p1, Owned Player1), (p2, Owned Player2), (n, Neutral)] of
      [_, (lForce, _), (wForce, winner)]
        | wForce == lForce -> Planet owner 0 growth
        | otherwise        -> Planet winner (wForce - lForce) growth
      _ -> error "impossible"

modifyGameState :: (GameState -> GameState) -> Game ()
modifyGameState f = modify (\st -> st { gameState = f (gameState st) })

arrival :: Game ()
arrival = do
  GameState planets _ fleets <- gets gameState
  let (arrived, fleets') = partition (\(Fleet _ _ _ turns) -> turns == 0) fleets

  modifyGameState (\(GameState ps ws _) -> GameState ps ws fleets')

  let initialForces :: Map PlanetId PlanetForces
      initialForces = M.map existingForces planets

  arrivingForces <- forM arrived $ \(Fleet owner ships viaWormhole _) -> do
    Wormhole _ (Target dst) _ <- gets (lookupWormhole viaWormhole)
    return (dst, fleetForce owner ships)

  let totalForces
        = M.unionWith (<>) initialForces (M.fromListWith (<>) arrivingForces)

  forM_ (M.toList totalForces) $ \ (pId, pForces) -> do
    modify (updatePlanet pId (settle pForces))

data Result
  = Player1Won
  | Player2Won
  | Draw
  | Interrupted

checkEndGame :: Game (Maybe Result)
checkEndGame = do
  turns <- gets remainingTurns

  p1Ships <- playerTotalShips Player1
  p2Ships <- playerTotalShips Player2

  let end = or [turns == 0, p1Ships == 0, p2Ships == 0]

  if end then
    return (Just (gameOver p1Ships p2Ships))
  else
    return Nothing

gameOver :: Ships -> Ships -> Result
gameOver p1Ships p2Ships
  | p1Ships > p2Ships = Player1Won
  | p1Ships < p2Ships = Player2Won
  | otherwise         = Draw

updateServerState :: Game ()
updateServerState
  = modify (\st -> st { remainingTurns = remainingTurns st - 1 })

--------------------------------------------------------------------------------
-- * Drawing

drawGameState :: (?radius :: Int, ?seed :: StdGen) => Game ()
drawGameState = do
  ServerState _ t1 t2 (GameState planets wormholes fleets) positions <- get
  liftIO $ drawMap (positions, planets, wormholes) fleets
  drawStats t1 t2
  liftIO $ moveCursor (mapToScreen (?radius, ?radius))

drawStats :: (?radius :: Int, ?seed :: StdGen)
  => IORef NominalDiffTime -> IORef NominalDiffTime -> Game ()
drawStats t1 t2 = do
  timer1 <- liftIO $ readIORef t1
  timer2 <- liftIO $ readIORef t2
  let stats
        = [ (resetCol, "Seed",            pure (showSeed ?seed))
          , (resetCol, "Remaining turns", show <$> getTurns)
          , (setP1Col, "Player 1 ships",  show <$> playerTotalShips Player1)
          , (setP1Col, "Player 1 growth", show <$> playerTotalGrowth Player1)
          , (setP1Col, "Player 1 time",   pure (show timer1))
          , (setP2Col, "Player 2 ships",  show <$> playerTotalShips Player2)
          , (setP2Col, "Player 2 growth", show <$> playerTotalGrowth Player2)
          , (setP2Col, "Player 2 time",   pure (show timer2))
          ]

  forM_ (zip [1..] stats) $ \(pos, (set_col, legend, value)) -> do
    v <- value
    liftIO $ do
      set_col
      moveCursor (?radius * 4 + 5, pos)
      putStr (legend ++ ": ")
      putStr "\ESC7"
      putStr "        "
      putStr "\ESC8"
      putStr v

