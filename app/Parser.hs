module Parser where

import Lib

import Text.Parsec
import Text.Parsec.String
import Text.Printf

parseComm :: String -> Maybe Constraint
parseComm = unWrap  . parse commParser ""
  where
    unWrap (Left _) = Nothing
    unWrap (Right cons) = Just cons 

commParser :: Parser Constraint
commParser = setShiftComm <|> setnotShiftComm

setShiftComm :: Parser Constraint
setShiftComm = do
  try $ string "set "
  spaces
  d <- day
  spaces
  maybeShift <- shift
  spaces
  emp <- many1 letter

  case maybeShift of
    Nothing -> return $ isFreeOn emp . d
    Just s -> return $ (==emp) . s . d

setnotShiftComm :: Parser Constraint
setnotShiftComm = do
  str <- try $ string "setnot "
  spaces
  d <- day
  spaces
  maybeShift <- shift
  spaces
  emp <- many1 letter

  case maybeShift of
    Nothing -> return $ not . isFreeOn emp . d
    Just s -> return $ (/=emp) . s . d

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
  s <- choice $ string <$> ["morning", "afternoon", "evening", "night", "free"]
  return $ case s of
    "morning" -> Just morning
    "afternoon" -> Just afternoon
    "evening" -> Just evening
    "night" -> Just night
    "free" -> Nothing