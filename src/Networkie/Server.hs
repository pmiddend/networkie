{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude()
import ClassyPrelude
import           Control.Concurrent         (forkFinally, forkIO,myThreadId,throwTo)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, put)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Map.Strict            as Map
import qualified Data.Text as T
import           Network.Socket             (Family (..),
                                             SocketType (..), accept,
                                             defaultProtocol, listen, socket,
                                             bindSocket,withSocketsDo,socketToHandle,getAddrInfo,AddrInfoFlag(..),defaultHints,AddrInfo(..))
import           System.IO                  (IOMode(..))
import           Text.Printf                (printf)

type PlayerName = T.Text

type World = Int

initialWorld :: World
initialWorld = 1

-- Input vom Client (noch nicht weiterzugeben an den Server)
data ClientInput = ClientInputName PlayerName
                 | ClientInputOther T.Text
                 deriving(Show)

-- Nachricht vom Client an den Server
data ServerCommand = SCommandNewClient PlayerName (Chan ClientCommand)
                   | SCommandClientRemove PlayerName
                   | SCommandClientInput PlayerName T.Text

-- Nachricht  vom Server an den Client
data ClientCommand = CCommandWorldUpdate World
                   | CCommandReject
                   | CCommandAccept PlayerName
                   deriving(Show)

data PerClientData = PerClientData {
    cdName :: T.Text
  , cdClientChan :: Chan ClientCommand
  }

type ServerCommandChan = Chan ServerCommand

{-
data ServerState = ServerState {
    sdMainChannel :: Chan ServerCommand
  {-, sdClients     :: Map.Map PlayerName PerClientData
  , sdWorld       :: World-}
  }
-}

data ClientState = ClientState {
    csPlayerName      :: Maybe PlayerName
  , csReadChan        :: Chan (Either ClientInput ClientCommand)
  , csToServerChan    :: Chan ServerCommand
  , csClientChan      :: Chan ClientCommand
  , csSocketWriteChan :: Chan T.Text
  }

type ClientMainLoopState a = StateT ClientState IO a

serverWaitForPlayers :: ServerCommandChan -> [PerClientData] -> IO ()
serverWaitForPlayers commandChan clients = do
  command <- readChan commandChan
  case command of
   SCommandNewClient pn clientchan -> do
     case find ((== pn) . cdName) clients of
      Just _ -> do putStrLn "Client already exists, rejecting"
                   writeChan clientchan CCommandReject
                   serverWaitForPlayers commandChan clients
      Nothing -> do putStrLn "Client is new, inserting"
                    let clients' = PerClientData pn clientchan : clients
                    writeChan clientchan (CCommandAccept pn)
                    if length clients == 2
                      then serverGame commandChan clients initialWorld
                      else serverWaitForPlayers commandChan clients'
   SCommandClientInput _ _ -> do
     serverWaitForPlayers commandChan clients
   SCommandClientRemove pn -> do
     serverWaitForPlayers commandChan (filter ((== pn) . cdName) clients)

serverGame :: ServerCommandChan -> [PerClientData] -> World -> IO ()
serverGame commandChan clients world = do
    mapM_ (\c -> writeChan (cdClientChan c) (CCommandWorldUpdate world)) clients
    serverGame'
  where serverGame' = do
          command <- readChan commandChan
          case command of
            SCommandNewClient _ clientchan -> do
              writeChan clientchan CCommandReject
              serverGame commandChan clients world
            SCommandClientInput name ip -> do
              putStrLn (name <> ": got client input: " <> tshow ip)
              let newWorld = world + 1
              forM_ clients (\c -> writeChan (cdClientChan c) (CCommandWorldUpdate newWorld))
            SCommandClientRemove pn -> do
              mapM_ (\c -> writeChan (cdClientChan c) CCommandReject) (filter ((/= pn) . cdName) clients)
              serverWaitForPlayers commandChan []


clientRead :: Handle -> Chan ClientInput -> IO ()
clientRead h inputChan = forever $ do
  line <- (T.pack . BS8.unpack) <$> BS8.hGetLine h
  if "name " `isPrefixOf` line
    then writeChan inputChan (ClientInputName (drop 5 line))
    else writeChan inputChan (ClientInputOther line)

readChans :: Chan a -> Chan b -> IO (Chan (Either a b))
readChans a b = do
  result <- newChan
  _ <- forkIO (forever $ readChan a >>= (writeChan result . Left))
  _ <- forkIO (forever $ readChan b >>= (writeChan result . Right))
  return result

clientWrite :: Handle -> Chan T.Text -> IO ()
clientWrite h c = forever $ do
  line <- readChan c
  BS8.hPut h (encodeUtf8 line)

clientHandler :: Handle -> Chan ServerCommand -> IO ()
clientHandler h toServerChan = do
  inputChan <- newChan
  tid <- myThreadId
  _ <- forkFinally (clientRead h inputChan) (\(Left e) -> throwTo tid e)
  clientWriteChan <- newChan
  _ <- forkIO (clientWrite h clientWriteChan)
  -- Warte auf Input vom Client auf Socket oder vom Server
  clientChan <- newChan
  mpChan <- readChans inputChan clientChan
  evalStateT clientMainLoop (ClientState Nothing mpChan toServerChan clientChan clientWriteChan)

clientWriteToServer :: ServerCommand -> ClientMainLoopState ()
clientWriteToServer c = do
  toServerChan <- gets csToServerChan
  writeChan toServerChan c

clientWriteToClient :: T.Text -> ClientMainLoopState ()
clientWriteToClient s = do
  clientChan <- gets csSocketWriteChan
  writeChan clientChan s

clientMainLoop :: ClientMainLoopState ()
clientMainLoop = forever $ do
  state <- get
  n <- readChan (csReadChan state)
  case n of
    Left clientInput ->
      case clientInput of
       ClientInputName name -> clientWriteToServer (SCommandNewClient name (csClientChan state))
       ClientInputOther other ->
         case (csPlayerName state) of
           Nothing -> clientWriteToClient "Please enter a name first\n"
           Just name -> clientWriteToServer (SCommandClientInput name other)
    Right clientCommand ->
      case clientCommand of
        CCommandWorldUpdate w -> clientWriteToClient ("New world: " <> tshow w <> "\n")
        CCommandReject -> clientWriteToClient ("Greeting rejected\n")
        CCommandAccept name -> do clientWriteToClient ("Greetings " <> name <> "!\n")
                                  put (state { csPlayerName = Just name })

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
     bindSocket sock (addrAddress serveraddr)
     listen sock myport
     printf "Listening on port %d\n" myport
     serverChan <- newChan
     _ <- forkIO (serverWaitForPlayers serverChan [])
     forever $ do
       (clientSocket, addr) <- accept sock
       h <- socketToHandle clientSocket ReadWriteMode 
       printf "Accepted connection from: %s\n" (show addr)
       forkFinally (clientHandler h serverChan) (\_ -> putStrLn "Closing client handle..." >> hClose h)
   Nothing -> putStrLn "Couldn't get port addrinfo, exiting"
