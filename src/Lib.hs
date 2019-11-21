module Lib where

type Employee = String
type Staff = [Employee]
type Constraint = WorkWeek -> Bool
type WeekGenerator = Staff -> [WorkWeek]

data WorkDay = WorkDay
  { morning :: Employee
  , afternoon :: Employee
  , evening :: Employee
  , night :: Employee
  } deriving (Eq, Show)

data WorkWeek = WorkWeek
  { sunday :: WorkDay
  , monday :: WorkDay
  , tuesday :: WorkDay
  , wednesday :: WorkDay
  } deriving (Eq, Show)

isFreeOn :: Employee -> WorkDay -> Bool
isFreeOn w (WorkDay m a e n) = w `notElem` [m, a, e, n]

choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n
  = [x:comb | comb <- choose xs (n-1)] ++ choose xs n

genSchedules :: WeekGenerator -> Staff -> [Constraint] -> [WorkWeek]
genSchedules src s cs = filter (\x -> all ($ x) cs) (src s)
