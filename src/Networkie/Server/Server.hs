{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           ClassyPrelude
import           Control.Concurrent         (forkIO)
import qualified Data.Text                  as T
import           Network.Socket             (AddrInfo (..), AddrInfoFlag (..),
                                             SocketType (..),
                                             bindSocket, defaultHints,
                                             defaultProtocol, getAddrInfo,
                                             listen, socket,
                                             withSocketsDo,setSocketOption,SocketOption(..))
import           Prelude                    ()
import           Text.Printf                (printf)
import Networkie.Server.ClientHandling(acceptLoop)
import Networkie.Server.Common

initialWorld :: World
initialWorld = 1

myMaxClients :: Int
myMaxClients = 2

data PerClientData = PerClientData {
    cdName       :: T.Text
  , cdClientChan :: Chan ClientCommand
  }

type Server = Chan ServerCommand

sendToClient :: ClientCommand -> Client -> IO ()
sendToClient command clientChan = writeChan clientChan command

readServerCommand :: Server -> IO ServerCommand
readServerCommand commandChan = readChan commandChan

broadcast :: ClientCommand -> [PerClientData] -> IO ()
broadcast command = mapM_ (sendToClient command . cdClientChan)

type PrePhaseCallback = IO ()
type MainPhaseCallback = [PerClientData] -> IO ()

type ClientsToWaitFor = Int

{-|
 - Initial State: Wait for 'n' player, then defer to main game loop
 -}
waitForPlayers :: Server ->
                  [PerClientData] ->
                  ClientsToWaitFor ->
                  MainPhaseCallback ->
                  IO ()
waitForPlayers commandChan clients maxClients callback = do
  command <- readServerCommand commandChan
  case command of
   SCommandNewClient pn client -> do
     case find ((== pn) . cdName) clients of
      Just _ -> do putStrLn "Client already exists, rejecting"
                   sendToClient CCommandReject client
                   waitForPlayers commandChan clients maxClients callback
      Nothing -> do putStrLn "Client is new, inserting"
                    let clients' = PerClientData pn client : clients
                    sendToClient (CCommandAccept pn) client
                    if length clients' == maxClients
                      then callback clients'
                      else waitForPlayers commandChan clients' maxClients callback
   SCommandClientInput _ _ -> do
     waitForPlayers commandChan clients maxClients callback
   SCommandClientRemove pn -> do
     waitForPlayers commandChan (filter ((== pn) . cdName) clients) maxClients callback

updateWorld :: World -> PlayerName -> GameClientInput -> World
updateWorld = undefined

runGame :: World ->
           Server ->
           [PerClientData] ->
           PrePhaseCallback -> IO ()
runGame world commandChan clients preCallback = do
    broadcast (CCommandWorldUpdate world) clients
    runGame'
  where runGame' = do
          command <- readServerCommand commandChan
          case command of
            SCommandNewClient _ clientchan -> do
              sendToClient CCommandReject clientchan
              runGame'
            SCommandClientInput name ip -> do
              putStrLn (name <> ": got client input: " <> tshow ip)
              let newWorld = world + 1
              broadcast (CCommandWorldUpdate newWorld) clients
              runGame'
            SCommandClientRemove pn -> do
              mapM_ (sendToClient CCommandReject . cdClientChan) (filter ((/= pn) . cdName) clients)
              preCallback

myport :: Int
myport = 31337

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just (show myport))
  case headMay addrinfos of
   Just serveraddr -> do
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     setSocketOption sock ReuseAddr 1
     bindSocket sock (addrAddress serveraddr)
     listen sock myport
     printf "Listening on port %d\n" myport
     serverChan <- newChan
     let waitState = waitForPlayers serverChan [] myMaxClients mainState
         mainState clients = runGame initialWorld serverChan clients waitState
     _ <- forkIO waitState
     acceptLoop sock serverChan
   Nothing -> putStrLn "Couldn't get port addrinfo, exiting"
