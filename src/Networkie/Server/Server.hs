{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Networkie.Server.ServerFramework
import           ClassyPrelude
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T
import           Network.Socket        (withSocketsDo)
import qualified Data.List.NonEmpty as NE
import           Prelude               (reads)
import Networkie.List(list)

{-
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
-}

myport :: Int
myport = 31337

readsToIncomingConversion :: Read a => IncomingDataConversion a
readsToIncomingConversion = fmap (second BS8.pack . NE.head) . NE.nonEmpty . reads . BS8.unpack

showOutgoingConversion :: Show a => OutgoingDataConversion a
showOutgoingConversion = BS8.pack . show

textUnpack :: BS8.ByteString -> Maybe (T.Text,BS8.ByteString)
textUnpack bs | BS8.null bs = Nothing
              | otherwise = Just (T.pack . BS8.unpack $ bs,mempty)

data TestData = Foo | Bar deriving(Read,Show)

main :: IO ()
main = withSocketsDo $ do
  --s' <- runServer myport textUnpack (BS8.pack . T.unpack)
  s' <- runServer myport readsToIncomingConversion showOutgoingConversion
  case s' of
   Left e -> error (T.unpack e)
   Right s -> do
     forever $ do
       input <- readCommand s
       print input
       case input of
         NewClient _ -> putStrLn "new client!"
         ClientClosed _ -> putStrLn "Oh :("
         IncomingData _ d -> case d of
           Foo -> putStrLn "Got foo"
           Bar -> putStrLn "Got bar"
