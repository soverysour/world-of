{-# LANGUAGE TupleSections #-}

module World
  ( createWorld
  , tickWorld
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

createWorld :: Dim -> Dim -> Dim -> WithRandT Keyed World
createWorld sizeX sizeY zones = do
  roughMap <- mkVoronoi sizeX sizeY zones
  mapAreas <- traverse mkMapArea $ cells roughMap
  key <- nextSeq
  pure $ World sizeX sizeY mapAreas [Indexed mkUselessConsumer (3, 3) key] mempty

tickWorld :: TimeWindow -> (Time, World) -> WithRandT Keyed (Time, World)
tickWorld timeWindow (time, world) =
  let finalTime = time + fromIntegral timeWindow
      interval = [time + 1 .. finalTime]
   in (finalTime, ) <$> foldM interpretRec world interval

interpretRec :: World -> Time -> WithRandT Keyed World
interpretRec initial time = exhaust afterPulse
  where
    afterPulse = interpret (False, Left Pulse) (const True, const . const True) time initial
    exhaust world = do
      world' <- world
      case SL.uncons $ events world' of
        Nothing -> pure world'
        Just ((fromArea, Timed (Indexed event location key) time'), tl) ->
          if time' > time
            then pure world'
            else exhaust $
                 interpret
                   (fromArea, Right event)
                   ((== location), \p k -> p == location && k /= key)
                   time
                   (world' {events = tl})

interpret :: MaybeFromArea Pulser -> (AreaLocalizer, ActorLocalizer) -> Time -> World -> WithRandT Keyed World
interpret (fromArea, e) (areaf, actorf) time (World x y areas actors events) = do
  (pulseActorEvents, pulsedActors) <- pulseActorsWith e actorf actors
  (pulseAreaEvents, pulsedMap) <-
    if fromArea
      then pure ([], areas)
      else pulseAreasWith e areaf areas
  let allEvents = ((False, ) <$> pulseActorEvents) <> ((True, ) <$> pulseAreaEvents)
      timedEvents = fmap (\event -> Timed event (fromIntegral (timeEvent $ unElement event) + time)) <$> allEvents
  pure $ World x y pulsedMap pulsedActors (SL.toSortedList timedEvents <> events)

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
      pure (worldEvents'' ++ worldEvents, worldActors'' ++ worldActors)

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
      pure (worldEvents', worldArea)
    convPos = bimap fromIntegral fromIntegral

indexing :: Point -> [Indexed a] -> a -> WithRandT Keyed [Indexed a]
indexing position acc event = do
  key <- nextSeq
  let i = Indexed event position key
  pure $ i : acc

nextSeq :: WithRandT Keyed Key
nextSeq = lift nextSeq'
  where
    nextSeq' = do
      number <- get
      put $ number + 1
      pure number

type AreaLocalizer = Point -> Bool

type ActorLocalizer = Point -> Key -> Bool
