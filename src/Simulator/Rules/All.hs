module Simulator.Rules.All where

import Simulator.Rules
import Simulator.Rules.AssignJob

rules :: [Rule]
rules = [assignJob]
