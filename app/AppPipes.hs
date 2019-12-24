module AppPipes
  ( tickerHandler
  , simulationHandler
  , gameHandler
  ) where

import           Pipes
import           Protolude
import           System.Random.SplitMix

import           Message
import           Types.Common
import           Types.World
import           World

tickerHandler :: TimeWindow -> Producer TickerMsg IO ()
tickerHandler window = do
  yield StartTick
  lift $ threadDelay (1000000 * fromIntegral window)
  yield EndTick
  tickerHandler window

simulationHandler ::
     TimeWindow -> (Time, World) -> SMGen -> Key -> Pipe (Either TickerMsg SimulationRequest) SimulationResult IO ()
simulationHandler window worldWithTime generator nextKey = do
  _ <- await
  yield Whatever
  print ("Hello From simulation" :: Text)
  let ((worldWithTime', generator'), nextKey') = runState (runStateT (tickWorld window worldWithTime) generator) nextKey
  simulationHandler window worldWithTime' generator' nextKey'

gameHandler :: Chan InToOutEvent -> Pipe (Either SimulationResult OutToInEvent) SimulationRequest IO ()
gameHandler chan = do
  _ <- await
  print ("Hello from Game Handler." :: Text)
  gameHandler chan
