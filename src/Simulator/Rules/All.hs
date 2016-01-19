module Simulator.Rules.All where

import Simulator.Rules
import Simulator.Rules.AssignJob
import Simulator.Rules.Assign

rules :: [Rule]
rules = [assignJob, assignV, assignL, assignI]
