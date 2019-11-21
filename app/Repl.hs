module Repl
( repl
) where

import Lib
import qualified Parser as P

import Text.Parsec (parse)
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
  putStrLn "Calculating..."
  print $ length wks
  print $ head wks
  writeFile "schedule.md" (prettyPrint $ head wks)
  putStrLn "Any conditions?"
  putStr "syntax: <day of the week> <shift name/\"free\"> <name>\n> "
  response <- getLine
  if response == "done"
    then 
      writeFile "schedule.md" (prettyPrint $ head wks)
    else
      let (strDay : strShift : employee : rest) = words response
          notMaybe = if rest == ["not"] then not else id
          shift = fromRight $ parse P.shift "" strShift
          day = fromRight $ parse P.day "" strDay
          newWks = filter (notMaybe . applyCond day shift employee) wks
      in
      if null newWks
        then putStrLn "Impossible. Try again or use a different solution." >>
             repl wks
        else repl newWks
  where
    applyCond day Nothing employee wk = employee `isFreeOn` day wk
    applyCond day (Just shift) employee wk = shift (day wk) == employee

    -- quick & dirty temp fix
    fromRight (Right x) = x