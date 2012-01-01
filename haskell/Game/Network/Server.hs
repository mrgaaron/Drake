module Main where 

import Prelude hiding (catch)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket, sClose)
import System (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, putStrLn, hClose)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Exception (onException, finally, catch, IOException)

import qualified Game.GameObjects.Room as RoomObject
import qualified Game.Parsers.Room as RoomParser

dataPath = "C:\\haskell\\game\\Data"

main :: IO ()
main = withSocketsDo $ do
	args <- getArgs
	let port = fromIntegral (read $ head args :: Int)
	sock <- listenOn $ PortNumber port
	putStrLn $ "Listening on " ++ (head args)
	acceptLoop sock `finally` (sClose sock)
	
acceptLoop socket = do
	(handle, host, port) <- accept socket
	hSetBuffering handle NoBuffering
	putStrLn $ "Accepting connection from " ++ host
	forkIO $ gameLoop handle
		`onException` cleanUp handle host 
	acceptLoop socket

handleCommandIOException :: IOException -> IO String
handleCommandIOException e = return "logout"

cleanUp handle host = do
	hClose handle
	putStrLn $ "Closed connection from " ++ host
	
gameLoop :: Handle -> IO ()
gameLoop handle = do
	hPutStrLn handle "Welcome to Drake!\n\nPlease log in:"
	handleLogin handle
	commandProcessor handle

handleLogin :: Handle -> IO()		
handleLogin handle = do
	line <- hGetLine handle
	hPutStrLn handle "Login successful!"
	firstRoom <- RoomParser.parseRoomFile dataPath "Test" "Home"
	home <- atomically $ RoomObject.initRoom firstRoom
	rmstr <- RoomObject.showRoom home
	hPutStrLn handle rmstr

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
	line <- hGetLine handle `catch` handleCommandIOException
	let cmd = words line
	case (head cmd) of
		("echo") -> echoCommand handle cmd
		("add") -> addCommand handle cmd
		("logout") -> logoutCommand handle cmd
		_ -> do hPutStrLn handle "Unknown command"
	commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
    hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd = do
    hPutStrLn handle $ show $ (read $ cmd !! 1) + (read $ cmd !! 2)

logoutCommand :: Handle -> [String] -> IO ()
logoutCommand handle cmd = do
	hPutStrLn handle "Goodbye!"
	hClose handle
