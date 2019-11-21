module Repl
( repl
) where

import Lib
import qualified Parser as P

import System.IO
import Text.Printf

prettyPrint :: WorkWeek -> String
prettyPrint (WorkWeek s m t w) =
  let WorkDay m1 a1 e1 n1 = s
      WorkDay m2 a2 e2 n2 = m
      WorkDay m3 a3 e3 n3 = t
      WorkDay m4 a4 e4 n4 = w
  in 
    "| | Sun | Mon | Tue | Wed |\n"
    ++ "| --- | :---: | :---: | :---: | :---: |\n"
    ++ printf "| Morning   | %s | %s | %s | %s |\n" m1 m2 m3 m4
    ++ printf "| Afternoon | %s | %s | %s | %s |\n" a1 a2 a3 a4
    ++ printf "| Evening   | %s | %s | %s | %s |\n" e1 e2 e3 e4
    ++ printf "| Night     | %s | %s | %s | %s |\n" n1 n2 n3 n4

repl wks = do
  printf "Number of possible solutions: %d\n" (length wks)
  print $ head wks
  writeFile "schedule.md" (prettyPrint $ head wks)
  putStr "Any conditions?\n> "
  hFlush stdout
  response <- getLine
  if response == "done"
    then 
      return ()
    else
      case P.parseComm response of
      Just cons ->
        let newWks = filter cons wks in 
        if null newWks
          then do
            putStrLn "Impossible. Try again or use a different solution."
            repl wks
          else do
            putStrLn "Calculating..."
            repl newWks
      Nothing -> do
        putStrLn "Error: Invalid command."
        repl wks