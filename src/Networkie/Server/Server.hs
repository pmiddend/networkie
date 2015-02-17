{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Networkie.Server.ServerFramework
import           ClassyPrelude
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T
import           Network.Socket        (withSocketsDo)
import           Prelude               ()

{-
data IncomingCommand a = NewClient Handle
                       | ClientClosed Handle
                       | IncomingData Handle a
                       deriving(Show)

data OutgoingCommand a = CloseClient Handle
                       | SendData Handle a

type Port = Int

type IncomingDataConversion a = BS8.ByteString -> Maybe (a,BS8.ByteString)
type OutgoingDataConversion a = a -> BS8.ByteString

data Server a = Server {
    _sincomingChannel :: Chan (IncomingCommand a)
  , _soutgoingChannel :: Chan (OutgoingCommand a)
  , _sacceptThread    :: ThreadId
  , _scommandThread   :: ThreadId
  }

$(makeLenses ''Server)

readCommand :: Server a -> IO (IncomingCommand a)
readCommand s = readChan (s ^. sincomingChannel)

writeCommand :: Server a -> (OutgoingCommand a) -> IO ()
writeCommand s = writeChan (s ^. soutgoingChannel)

hGetAll :: Handle -> IO (Maybe BS8.ByteString)
hGetAll h = do
  result <- BS8.hGetSome h 1024
  if BS8.null result
    then return Nothing
    else return (Just result)

clientRead :: Handle ->
              Chan (IncomingCommand a) ->
              IncomingDataConversion a ->
              IO ()
clientRead h incomingChannel incomingConversion = clientRead' mempty
  where clientRead' buffer = do
          bytes' <- hGetAll h
          case bytes' of
           Nothing -> return ()
           Just bytes -> do
             let (as,rest) = unfoldRest incomingConversion (buffer <> bytes)
             mapM_ (writeChan incomingChannel . (IncomingData h)) as
             clientRead' rest

acceptLoop :: Socket ->
              Chan (IncomingCommand a) ->
              IncomingDataConversion a ->
              IO ()
acceptLoop sock incomingChannel incomingConversion = forever $ do
  (clientSocket, addr) <- accept sock
  h <- socketToHandle clientSocket ReadWriteMode
  hSetBuffering h NoBuffering
  printf "Accepted connection from: %s\n" (show addr)
  writeChan incomingChannel (NewClient h)
  let closeHandler _ = do putStrLn "Closing client handle..."
                          hClose h
                          writeChan incomingChannel (ClientClosed h)
  void $ forkFinally (clientRead h incomingChannel incomingConversion) closeHandler

commandLoop :: Chan (OutgoingCommand a) ->
               OutgoingDataConversion a ->
               IO ()
commandLoop outgoingChannel outgoingConversion = forever $ do
  command <- readChan outgoingChannel
  case command of
   CloseClient h -> hClose h
   SendData h a -> BS8.hPut h (outgoingConversion a)

runServer :: Port ->
             IncomingDataConversion a ->
             OutgoingDataConversion a ->
             IO (Either T.Text (Server a))
runServer port incomingConversion outgoingConversion = withSocketsDo $ do
  addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just (show port))
  case headMay addrinfos of
   Just serveraddr -> do
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     setSocketOption sock ReuseAddr 1
     bindSocket sock (addrAddress serveraddr)
     listen sock port
     printf "Listening on port %d\n" port
     incomingChannel <- newChan
     outgoingChannel <- newChan
     acceptThread <- forkFinally (acceptLoop sock incomingChannel incomingConversion) (\_ -> sClose sock)
     commandThread <- forkIO (commandLoop outgoingChannel outgoingConversion)
     return (Right (Server incomingChannel outgoingChannel acceptThread commandThread))
   Nothing -> return (Left "Couldn't get port addrinfo, exiting")

closeServer :: Server a -> IO ()
closeServer s = do
  killThread (s ^. sacceptThread)
  killThread (s ^. scommandThread)

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

-}
myport :: Int
myport = 31337

textUnpack :: BS8.ByteString -> Maybe (T.Text,BS8.ByteString)
textUnpack bs | BS8.null bs = Nothing
              | otherwise = Just (T.pack . BS8.unpack $ bs,mempty)

main :: IO ()
main = withSocketsDo $ do
  s' <- runServer myport textUnpack (BS8.pack . T.unpack)
  case s' of
   Left e -> error (T.unpack e)
   Right s -> do
     forever $ do
       input <- readCommand s
       print input
       case input of
         NewClient h -> writeCommand s (CloseClient h)
         ClientClosed _ -> putStrLn "Oh :("
         IncomingData _ a -> putStrLn a
                           

{-
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

-}
