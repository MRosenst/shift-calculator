module Main where

import Constraints
import CustomWeek
import Lib
import Repl
import StdWeek

import Control.Monad
import Text.Printf

staff = ["Daniel", "Lior", "Lital", "Moshe", "Omer", "Ron"]

main :: IO ()
main = do
  putStrLn "Standard solution?"
  ans <- getLine
  if 'y' `elem` ans || 'Y' `elem` ans
    then do
      putStrLn $ "Options: " ++ show staff
      putStrLn "Who worked last Shabbat?"
      p <- getLine
      putStrLn "Who works next Shabbat?"
      n <- getLine
      putStrLn "Who works Wednesday night?"
      w <- getLine
      let wks = genStdSchedules (StdWeek p n w []) staff
      repl wks
    else do
      putStrLn "Using custom solution..."
      let wks = genSchedules customWeekGen staff customConstraints
      repl wks
  
