module Main
  ( main
  ) where

import           Pipes
import           Pipes.Concurrent
import           Pipes.Prelude
import           Protolude              hiding (map)
import           System.Random.SplitMix

import           AppPipes
import           Config
import           Intro
import           Types.Common
import           Types.World
import           Websocket
import           World

main :: IO ()
main = runGame testServer testWorld

runGame :: ServerConfig -> WorldConfig -> IO ()
runGame serverConfig worldConfig = do
  generator <- newSMGen
  chanFromServer <- newChan
  chanToServer <- newChan
  --
  -- Setup the initial simulation configuration.
  --
  let ((result, generator'), nextKey) = runState (runStateT (initWorld worldConfig) generator) (startKey worldConfig)
  --
  -- Create the mailboxes for the application.
  --
  (putServerAndSim, takeServerAndSim) <- spawn unbounded
  (putGameAndTick, takeGameAndTick) <- spawn unbounded
  --
  -- Create some sources and some sinks.
  --
  let sinkServerAndSim = toOutput putServerAndSim
      sourceServerAndSim = fromInput takeServerAndSim
      sinkGameAndTick = toOutput putGameAndTick
      sourceGameAndTick = fromInput takeGameAndTick
      -- And the handlers.
      serverProducer = chanToPipe chanFromServer
      gameHandler' = gameHandler chanToServer
      ticker' = tickerHandler (tickPeriod worldConfig)
      simulationRunner' = simulationHandler (windowSize worldConfig) result generator' nextKey
  --
  -- Pipe the components together.
  --
  let ticker = ticker' >-> map Left >-> sinkGameAndTick
      serverWs = serverProducer >-> map Right >-> sinkServerAndSim
      simulationRunner = sourceGameAndTick >-> simulationRunner' >-> map Left >-> sinkServerAndSim
      serverEngine = sourceServerAndSim >-> gameHandler' >-> map Right >-> sinkGameAndTick
  --
  -- Run the effects (forked to other threads) and start the WS server.
  --
  runEffect' ticker
  runEffect' simulationRunner
  runEffect' serverEngine
  runEffect' serverWs
  webSocketServer chanFromServer serverConfig

initWorld :: WorldConfig -> WithRandT Keyed (Time, World)
initWorld (WorldConfig dimx dimy zoneCount timeWindow initialTime _ startSteps _) = do
  initialWorld <- createWorld dimx dimy zoneCount
  foreachM (tickWorld timeWindow) (initialTime, initialWorld) startSteps
