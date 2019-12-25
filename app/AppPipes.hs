module AppPipes
  ( tickerHandler
  , simulationHandler
  , gameHandler
  ) where

import           GHC.Conc               (par)
import           Pipes
import           Protolude
import           System.Random.SplitMix

import           Message
import           Types.Common
import           Types.World
import           World

tickerHandler :: TimeWindow -> Producer TickerMsg IO ()
tickerHandler window = do
  lift $ threadDelay (1000000 * fromIntegral window)
  yield Tick
  tickerHandler window

simulationHandler ::
     TimeWindow -> (Time, World) -> SMGen -> Key -> Pipe (Either TickerMsg SimulationRequest) SimulationResult IO ()
simulationHandler window worldWithTime generator nextKey = do
  element <- await
  print ("Hello from Sim Handler." :: Text)
  case element of
    Left Tick ->
      let ((newWorld, generator'), nextKey') = runState (runStateT (tickWorld window worldWithTime) generator) nextKey
       in newWorld `par` simulationHandler window newWorld generator' nextKey'
    Right _ -> simulationHandler window worldWithTime generator nextKey

gameHandler :: Pipe (Either SimulationResult OutToInEvent) (Either InToOutEvent SimulationRequest) IO ()
gameHandler = do
  _ <- await
  print ("Hello from Game Handler." :: Text)
  gameHandler
