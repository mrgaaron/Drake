module World where

import qualified Game.GameObjects.Room as RObj
--import qualified Game.Parsers.Room as rparse
import Control.Concurrent.STM
import qualified Data.Map as Map

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension, dropExtension)

type RoomMap = Map.Map String RObj.Room
type RoomList = TVar [RObj.Room]

data World = World {
	rooms		:: RoomList
	}
	
padString shorter longer = 
	let padLength = (length longer) - (length shorter) in
		shorter ++ (replicate padLength '\\')

getIDFromPath mainpath path =
	--the mainpath will always be shorter than the path to the room file
	--so we can pad it to make zip work correctly
	dropExtension $ snd $ unzip . dropWhile compLetters $ zip (padString mainpath path) path where
		compLetters (a,b) = a == b
		
getRooms dir = do
	names <- getDirectoryContents dir
	let properNames = filter (`notElem` [".", ".."]) names
	paths <- forM properNames $ \name -> do
		let path = dir </> name
		isDirectory <- doesDirectoryExist path
		case isDirectory of
			True -> getRooms path
			False -> do
				let ext = takeExtension path
				if ext == ".rm"
					then return [path]
					else return []
	return $ concat paths


