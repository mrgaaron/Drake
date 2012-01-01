import qualified Game.GameObjects.Room as RoomObject
import qualified Game.Parsers.Room as RoomParser
import Control.Concurrent.STM

defaultRoom = "Test/Home"
dataPath = "C:\\haskell\\game\\Data"

main = do 
	contents <- RoomParser.parseRoomFile dataPath "Test" "Home"
	home <- atomically $ RoomObject.initRoom contents
	RoomObject.showRoom home