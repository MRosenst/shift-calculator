module Parser where

import Lib

import Text.Parsec
import Text.Parsec.String
import Text.Printf

day :: Parser (WorkWeek -> WorkDay)
day = do
  s <- choice $ string <$> ["sunday", "monday", "tuesday", "wednesday"]
  return $ case s of
    "sunday" -> sunday
    "monday" -> monday
    "tuesday" -> tuesday
    "wednesday" -> wednesday

shift :: Parser (Maybe (WorkDay -> Employee))
shift  = do
  s <- choice $ string <$> ["morning", "afternoon", "evening", "nigh", "free"]
  return $ case s of
    "morning" -> Just morning
    "afternoon" -> Just afternoon
    "evening" -> Just evening
    "night" -> Just night
    "free" -> Nothing