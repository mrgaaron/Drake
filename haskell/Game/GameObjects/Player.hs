{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.GameObjects.Player 
    ( 
      Player(..),
      Item(..),
      Gold,
      HitPoint,
      newPlayer
    ) where 

import Control.Concurrent.STM
import Control.Monad

data Item = Scroll
          | Wand
          | Banjo
            deriving (Eq, Ord, Show)

newtype Gold = Gold Int
    deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
    deriving (Eq, Ord, Show, Num)

type CName = TVar String
type Inventory = TVar [Item]
type Health = TVar HitPoint
type Balance = TVar Gold

data Player = Player {
	name		:: CName,
      	balance 	:: Balance,
      	health 		:: Health,
      	inventory 	:: Inventory
    	}

newPlayer :: String -> Gold -> HitPoint -> [Item] -> STM Player
newPlayer name gld hlth inv = do
	Player `liftM` 	newTVar name
		  `ap`  newTVar gld
		  `ap`  newTVar hlth
		  `ap`  newTVar inv

alterValue :: TVar a -> a -> STM ()
alterValue tvar val = do
	writeTVar tvar val

transfer :: Num a => a -> TVar a -> TVar a -> STM ()
transfer qty fromBal toBal = do
	fromQty <- readTVar fromBal
  	toQty   <- readTVar toBal
  	writeTVar fromBal (fromQty - qty)
  	writeTVar toBal   (toQty + qty)

