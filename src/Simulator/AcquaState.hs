module Simulator.AcquaState where

import qualified Data.Map as Map

data StateValue = IntVal Int
                | MapIntInt (Map.Map Int Int)
                deriving (Eq,Show)

type AcquaState = Map.Map String StateValue

statesDefault :: AcquaState
statesDefault = Map.fromList [
    ("envId",IntVal 0),
    ("occupiedPUPerCycle", MapIntInt (Map.fromList [])),
    ("maxQueueSize", IntVal 0),
    ("maxCallRec", IntVal 0),
    ("maxLocalCallRec", IntVal 0),
    ("maxMsg", IntVal 0)
  ]

getNextEnvId :: Map.Map String StateValue -> (String, Map.Map String StateValue)
getNextEnvId s = (nextEnvId,s')
  where
    Just (IntVal count) = Map.lookup "envId" s
    s' = Map.insert "envId" (IntVal (count+1)) s
    nextEnvId = "env_" ++ (show count)


