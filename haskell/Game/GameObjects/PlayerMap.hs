module Game.GameObjects.PlayerMap 
	(
		lookupPlayer,
		registerPlayer,
		readRegister,
		updateRegister
	) where

import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Monad (liftM, ap)
import System.IO.Unsafe(unsafePerformIO)
import Game.GameObjects.Player

type PlayerMap = Map.Map String Player

-- use unsafe IO to get a top level TVar to represent the
-- player registry so we have it in one place
pReg = unsafePerformIO $ newTVarIO (Map.empty :: PlayerMap)

lookupPlayer :: String -> STM (Maybe Player)
lookupPlayer name = do
	rg <- readRegister
	Map.lookup name `liftM` rg

registerPlayer :: Player -> STM Bool
registerPlayer pl = do
	key <- readTVar (name pl)
	mp <- readTVar pReg
	updateRegister (Map.insert key pl mp)
	return True	

readRegister :: STM (STM PlayerMap)
readRegister = do
	return (readTVar pReg)

updateRegister :: PlayerMap -> STM ()
updateRegister mp = do
	writeTVar pReg mp

