module Game.GameObjects.Room
    (
      Room(..),
	  Terrain(..),
	  Weather(..),
	  checkForExit,
	  getDestination,
	  initRoom,
	  showRoom
    ) where

import Control.Concurrent.STM
import Control.Monad(liftM, ap, forM)
import System.IO
import Data.List(find, intersperse)
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO.Unsafe(unsafePerformIO)

type RID = TVar String
type RName = TVar String
type RDescription = TVar String
type RTerrain = TVar Terrain
type RWeather = TVar Weather
type RExits = TVar [String]

data Terrain = Plains
		| Mountainous
		| Forested
		| Urban
		| Underground
		| Aquatic
		| Arid
		deriving (Show)

data Weather = Clear
		| Cloudy
		| Rainy
		| Snowy
		| Thunderstorming
		| Windy
		deriving (Show)

data Room = Room {
	roomid		:: RID,
	title		:: RName,
	description	:: RDescription,
	terrain		:: RTerrain,
	weather		:: RWeather,
	exits		:: RExits
	}

terrainFromString "u" 	= Just Urban
terrainFromString "m" 	= Just Mountainous
terrainFromString "f" 	= Just Forested
terrainFromString "p" 	= Just Plains
terrainFromString "un" 	= Just Underground
terrainFromString "aq" 	= Just Aquatic
terrainFromString "ar" 	= Just Arid
terrainFromString _   	= Nothing

alterValue :: TVar a -> a -> STM ()
alterValue tvar val = do
	writeTVar tvar val
	
extractDir = takeWhile (\x -> x /= '-')
extractDest = tail . (dropWhile (\x -> x /= '-'))

checkForExit exitName exitList =
	exitName `elem` (map extractDir exitList)

getDestination exitName exitList = do
	exit <- find ((==) exitName . extractDir) exitList
	return $ extractDest exit
	
buildRoom rm chunk = do
	case (chunk !! 0) 
		of "Title" -> alterValue (title rm) (chunk !! 1)
		   "Description" -> alterValue (description rm) (chunk !! 1)
		   "Terrain" -> case terrainFromString $ chunk !! 1 of
				Just x -> alterValue (terrain rm) (x)
				Nothing -> alterValue (terrain rm) (Plains)
		   "Exits" -> alterValue (exits rm) (words $ chunk !! 1)
		   otherwise -> alterValue (weather rm) (Clear)

initRoom :: [[String]] -> STM Room
initRoom chunks = do
	newrm <- newRoom "Test" "dummy" "dummy" Plains Clear ["North-Test/1", "South-Test/2"]
	forM chunks (buildRoom newrm)
	return newrm
	
newRoom :: String -> String -> String -> 
			Terrain -> Weather -> [String] -> STM Room
newRoom rmid ttl desc trn wthr exits = do
	Room 	`liftM` 	newTVar rmid
		`ap`	   	newTVar ttl
		`ap`   		newTVar desc
		`ap`    	newTVar trn
		`ap`    	newTVar wthr
		`ap`    	newTVar exits
		
exitsToString exits =
	concat . intersperse ", " $ map extractDir exits

showRoom rm = do
	rmTitle <- atomically $ readTVar (title rm)
	rmDesc <- atomically $ readTVar (description rm)
	exits <- atomically $ readTVar (exits rm)
	return $ rmTitle ++ "\n" ++ rmDesc ++ "\nExits: " ++ exitsToString exits