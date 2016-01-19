module Simulator.Rules.All where

import Simulator.Rules

-- queue rules
import Simulator.Rules.AssignJob
import Simulator.Rules.Call
import Simulator.Rules.NewEnv

import Simulator.Rules.Assign

rules :: [Rule]
rules = [assignJob, assignV, assignL, assignI, envNew, call]
