module Game.GameObjects.NPC
    (
      NPC(..),
    ) where

type ID = Int
type Name = String
type Health = Int
type Description = String

data NPC = NPC {
	npcID		:: ID,
	npcName		:: Name,
	npcHealth	:: Health,
	npcDescription	:: Description
	} deriving (Show)

changeNPCHealth :: NPC -> Int -> NPC
changeNPCHealth npc hlth = npc { npcHealth = hlth }

