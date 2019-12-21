module World
  ( createWorld
  , tickWorld
  , WorldConfig(..)
  , TimeWindow
  ) where

import qualified Data.Matrix                       as M
import qualified Data.SortedList                   as SL
import           Protolude

import           Generator.Areas
import           Generator.Voronoi
import           Types.Common
import           Types.Voronoi
import           Types.World
import           Types.World.Actor.UselessConsumer
import           World.Config

createWorld :: WorldConfig -> IO World
createWorld (WorldConfig sizeX sizeY zones _) = do
  roughMap <- mkVoronoiIO sizeX sizeY zones
  mapAreas <- traverse mkMapAreaIO $ cells roughMap
  return $ World sizeX sizeY mapAreas [Indexed mkUselessConsumer (3, 3) 1] mempty

tickWorld :: Time -> TimeWindow -> World -> WithRandT Keyed World
tickWorld time timeWindow world =
  let interval = [time + 1 .. time + fromIntegral timeWindow]
   in foldM interpretRec world interval

interpretRec :: World -> Time -> WithRandT Keyed World
interpretRec initial time = exhaust afterPulse
  where
    afterPulse = interpret (Left Pulse) (const True, const . const True) time initial
    exhaust world = do
      world' <- world
      case SL.uncons $ events world' of
        Nothing -> return world'
        Just (Timed (Indexed event location key) time', tl) ->
          if time' > time
            then return world'
            else exhaust $
                 interpret (Right event) ((== location), \p k -> p == location && k /= key) time (world' {events = tl})

interpret :: Pulser -> (AreaLocalizer, ActorLocalizer) -> Time -> World -> WithRandT Keyed World
interpret e (areaf, actorf) time (World x y areas actors events) = do
  (pulseActorEvents, pulsedActors) <- pulseActorsWith e actorf actors
  (pulseAreaEvents, pulsedMap) <- pulseAreasWith e areaf areas
  let allEvents = pulseActorEvents <> pulseAreaEvents
      timedEvents = fmap (\event -> Timed event (fromIntegral (timeEvent $ unElement event) + time)) allEvents
  return $ World x y pulsedMap pulsedActors (SL.toSortedList timedEvents <> events)

pulseActorsWith ::
     Pulser -> ActorLocalizer -> [Indexed WorldActor] -> WithRandT Keyed ([Indexed WorldEvent], [Indexed WorldActor])
pulseActorsWith e f = foldM mapFunc ([], [])
  where
    mapFunc (worldEvents, worldActors) actor = do
      (worldEvents', worldActors') <-
        if f (unPoint actor) (unKey actor)
          then fmap (foldM (indexing (unPoint actor)) []) <$> (unElement actor `acting` e)
          else pure ([], pure [actor])
      worldEvents'' <- foldM (indexing (unPoint actor)) [] worldEvents'
      worldActors'' <- worldActors'
      return (worldEvents'' ++ worldEvents, worldActors'' ++ worldActors)

pulseAreasWith ::
     Pulser -> AreaLocalizer -> M.Matrix WorldArea -> WithRandT Keyed ([Indexed WorldEvent], M.Matrix WorldArea)
pulseAreasWith e f = fmap sequence . traverse mapFunc . M.mapPos (,)
  where
    mapFunc (position, area) = do
      (worldEvents, worldArea) <-
        if f (convPos position)
          then area `interpreting` e
          else pure ([], area)
      worldEvents' <- foldM (indexing (convPos position)) [] worldEvents
      return (worldEvents', worldArea)
    convPos = bimap fromIntegral fromIntegral

indexing :: Point -> [Indexed a] -> a -> WithRandT Keyed [Indexed a]
indexing position acc event = do
  key <- nextSeq
  let i = Indexed event position key
  return $ i : acc

nextSeq :: WithRandT Keyed Key
nextSeq = lift nextSeq'
  where
    nextSeq' = do
      number <- get
      put $ number + 1
      return number

type AreaLocalizer = Point -> Bool

type ActorLocalizer = Point -> Key -> Bool
