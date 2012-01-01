module Main where

import Control.Concurrent
import Data.Char
import Network
import Network.Socket
import System.IO
import System(getArgs)

main :: IO ()
main = withSocketsDo $ do
	args <- getArgs     
	addrinfos <- getAddrInfo Nothing (Just (args !! 0)) (Just (args !! 1))       
	let serveraddr = head addrinfos       
	sock <- socket (addrFamily serveraddr) Stream defaultProtocol       
	connect sock (addrAddress serveraddr)       
	putStrLn "Connected to Drake server"       
	handle <- socketToHandle sock ReadWriteMode       
	hSetBuffering handle NoBuffering
	handler handle
	
handler handle = do
	forkIO $ readHandle handle
	handleCommands handle
	
handleCommands handle = do
	cmd <- getLine
	hPutStrLn handle cmd
	handleCommands handle
	
readHandle handle = do
	line <- hGetLine handle
	putStrLn line
	readHandle handle