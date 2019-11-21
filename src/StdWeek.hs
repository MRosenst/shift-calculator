module StdWeek 
  ( genStdSchedules
  , StdWeek (..)
  ) where

import Constraints
import Lib

import Data.List

data StdWeek = StdWeek
  { prevSab :: Employee
  , nextSab :: Employee
  , wedNight :: Employee
  , special :: [Constraint]
  }

genStdSchedules :: StdWeek -> WeekGenerator
genStdSchedules week s = genSchedules (stdWeekCombos week) s (mkConstraintList week)

mkConstraintList :: StdWeek -> [Constraint]
mkConstraintList (StdWeek p n w sp) = sp ++ [twoLateShifts, hasRest, prevSabCons p, nextSabCons n, wedNightCons w]

stdWeekCombos :: StdWeek -> WeekGenerator
stdWeekCombos (StdWeek p n w _) s = do
  [m1,a1,e1,n1] <- concatMap permutations (delete p s `choose` 4)
  [m2,a2,e2,n2] <- concatMap permutations (delete n1 s `choose` 4)
  [m3,a3,e3,n3] <- concatMap permutations (delete n2 s `choose` 4)
  [m4,a4,e4] <- permutations (s \\ [n, n3, w])
  let sun = WorkDay m1 a1 e1 n1
  let mon = WorkDay m2 a2 e2 n2
  let tues = WorkDay m3 a3 e3 n3
  let wed = WorkDay m4 a4 e4 w

  return $ WorkWeek sun mon tues wed

