module Constraints where

import Lib

import Data.List

hasRest :: Constraint
hasRest (WorkWeek s m t w) =
  night s `isFreeOn` m &&
  night m `isFreeOn` t &&
  night t `isFreeOn` w

worksLate :: Employee -> WorkDay -> Bool
worksLate employee d = evening d == employee || night d == employee

twoLateShifts :: Constraint
twoLateShifts (WorkWeek s m t w) = all (<=2) . map length . group . sort $ concatMap (\d -> [evening d, night d]) [s,m,t,w]

prevSabCons :: Employee -> Constraint
prevSabCons employee (WorkWeek s m t w) = length (filter (worksLate employee) [s,m,t,w]) <= 1 && employee `isFreeOn` s
  
nextSabCons :: Employee -> Constraint
nextSabCons employee (WorkWeek s m t w) = not (any (worksLate employee) [s,m,t,w]) && employee `isFreeOn` w

wedNightCons :: Employee -> Constraint
wedNightCons employee (WorkWeek s m t w) = night w == employee && not (any (worksLate employee) [s,m,t])
