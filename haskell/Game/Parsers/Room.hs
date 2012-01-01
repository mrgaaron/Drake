module Game.Parsers.Room
	(
		parseRoomFile
	) where

import Text.ParserCombinators.Parsec
import System.IO

-- A Room file consists of a tag followed by :
-- followed by the value for that field and then
-- % on its own line as a delimiter

roomFile = sepBy field eol
field = sepBy contents (string ":\n")
contents = many (noneOf ":%")
eol = string "%\n"
	
genFilePath rootPath rmpath rmid =
	rootPath ++ "\\" ++ (rmpath ++ "\\" ++ rmid ++ ".rm")
	
lookupRoom :: String -> String -> String -> IO String
lookupRoom rootPath region roomName =
	readFile $ genFilePath rootPath region roomName

parseRoom :: String -> Either ParseError [[String]]
parseRoom input = parse roomFile "(bad file format)" input

parseRoomFile rootPath region roomName = do
		contents <- lookupRoom rootPath region roomName
		case parseRoom contents of
			Left err  -> return []
			Right val -> return val
