module Weather.Parsers.Zone
  (
    parseSettingsFile
  )
 where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

settingsFile = endBy line eol
line = sepBy cell $ char ':'
cell = many $ noneOf ":\n\r"
eol =    try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\n"
     <|> string "\r"

flatten = foldl (++) []

finalize :: [[String]] -> [(String, Float)]
finalize input = flatten $  map convertToFloat $ filter notComment input where
		notComment a = (length a > 1)
		convertToFloat a = [((a !! 0), read (a !! 1) :: Float)]

parseSettingsFile path = do
	result <- parseFromFile settingsFile path
	case result of
		Left x -> return Map.empty
		Right x -> return $ Map.fromList $ finalize x
