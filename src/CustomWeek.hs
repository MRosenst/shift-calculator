module CustomWeek
  ( customConstraints
  , customWeekGen
  ) where

import Constraints
import Lib

import Data.List

customConstraints = [twoLateShifts, hasRest, prevSabCons "Daniel", prevSabCons "Lior", wedNightCons "Ron", moshe, omer, lior]

moshe :: Constraint
moshe (WorkWeek s m t w) =
  night m /= "Moshe" 
  && "Moshe" `isFreeOn` t 
  && "Moshe" `isFreeOn` w
  
omer :: Constraint
omer (WorkWeek s m t w) = morning t == "Omer"

lior :: Constraint
lior (WorkWeek s m t w) = afternoon m == "Lior"

customWeekGen :: WeekGenerator
customWeekGen s = do
  [m1,a1,e1,n1] <- concatMap permutations ((s \\ ["Lior", "Daniel"]) `choose` 4)
  [m2,a2,e2,n2] <- concatMap permutations ((s \\ [n1]) `choose` 4)
  [m3,a3,e3,n3] <- concatMap permutations ((s \\ [n2, "Moshe"]) `choose` 4)
  [m4,a4,e4]    <- concatMap permutations ((s \\ [n3, "Ron", "Daniel", "Moshe"]) `choose` 3)
  let sun = WorkDay m1 a1 e1 n1
  let mon = WorkDay m2 a2 e2 n2
  let tues = WorkDay m3 a3 e3 n3
  let wed = WorkDay m4 a4 e4 "Ron"

  return $ WorkWeek sun mon tues wed

