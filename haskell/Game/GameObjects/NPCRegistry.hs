module Game.GameObjects.NPCRegistry
    (
      getNPCs,
      insertNPC,
      insertRoom,
      lookupNPC,
    ) where

import qualified Data.Map as Map
import Control.Monad (liftM2)
import qualified Game.GameObjects.NPC as NPC
import qualified Game.GameObjects.Room as Room

type NPCRegistry = Map.Map ID [NPC.NPC]
type ID = Int

getNPCs :: NPCRegistry -> ID -> Maybe [NPC.NPC]
getNPCs reg id = Map.lookup id reg 

insertNPC reg id npc = do
	npcs <- liftM2 (:) (getNPCs reg id) npc
	return npcs

insertRoom reg rm = Map.insert (Room.roomid rm) [] reg

lookupNPC :: ID -> [NPC.NPC] -> [NPC.NPC]
lookupNPC id npcs = filter matchID npcs 
	where matchID npc = NPC.npcID npc == id
