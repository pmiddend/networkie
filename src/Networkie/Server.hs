{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude()
import ClassyPrelude
import           Control.Concurrent         (forkFinally, forkIO)
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

{-
Funktionsweise:

  - Server akzeptiert erstmal in einer Endlosschleife immer wieder Verbindungen.
  - Für jede geöffnete Verbindung werden drei Threads geforked: Einer liest, der andere schreibt, der dirte vermittelt.
  - Vorher wird allerdings auf den Spielernamen gewartet, erst danach werden die anderen beiden Threads gestartet.
  - Es gibt eine Hauptschleife, die auf den Kommandochannel wartet.
  - Ein Kommando ist: "neuer Client", "Client weggebrochen", "Client hat Daten empfangen"
  - Wurde der Name empfangen, wird die "neuer Client"-Nachricht an den Kommandochannel geschickt.
  - Der neue Client bekommt initial die Welt und verschickt sie.
-}

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
  cdClientChan :: Chan ClientCommand
  }

data ServerState = ServerState {
    sdMainChannel :: Chan ServerCommand
  , sdClients     :: Map.Map PlayerName PerClientData
  , sdWorld       :: World
  }

data ClientState = ClientState {
    csPlayerName      :: Maybe PlayerName
  , csReadChan        :: Chan (Either ClientInput ClientCommand)
  , csToServerChan    :: Chan ServerCommand
  , csClientChan      :: Chan ClientCommand
  , csSocketWriteChan :: Chan T.Text
  }

type ServerMainLoopState a = StateT ServerState IO a

type ClientMainLoopState a = StateT ClientState IO a

serverMainLoop :: ServerMainLoopState ()
serverMainLoop = forever $ do
  state <- get
  command <- readChan (sdMainChannel state)
  case command of
   SCommandNewClient pn clientchan -> do
     if pn `Map.member` (sdClients state)
       then do putStrLn "Client already exists, rejecting"
               writeChan clientchan CCommandReject
       else do putStrLn "Client is new, inserting"
               put (state { sdClients = Map.insert pn (PerClientData clientchan) (sdClients state) })
               writeChan clientchan (CCommandAccept pn)
               writeChan clientchan (CCommandWorldUpdate (sdWorld state))
   SCommandClientRemove pn -> do
     put (state { sdClients = Map.delete pn (sdClients state) })
   SCommandClientInput name ip -> do
     putStrLn (name <> ": got client input: " <> tshow ip)
     let newWorld = (sdWorld state) + 1
     put (state { sdWorld = newWorld })
     forM_ (Map.elems $ sdClients state) (\cd -> writeChan (cdClientChan cd) (CCommandWorldUpdate newWorld))

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
  _ <- forkIO (clientRead h inputChan)
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
     _ <- forkIO (evalStateT serverMainLoop (ServerState serverChan Map.empty initialWorld))
     forever $ do
       (clientSocket, addr) <- accept sock
       h <- socketToHandle clientSocket ReadWriteMode 
       printf "Accepted connection from: %s\n" (show addr)
       forkFinally (clientHandler h serverChan) (\_ -> putStrLn "Closing handle..." >> hClose h)
   Nothing -> putStrLn "Couldn't get port addrinfo, exiting"
