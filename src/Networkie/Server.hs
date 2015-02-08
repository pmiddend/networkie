{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           ClassyPrelude
import           Control.Concurrent         (forkFinally, forkIO, myThreadId,
                                             throwTo,ThreadId)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, put)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           FRP.Sodium                 (Event,newEvent,sync,execute,Behavior,accum,Reactive,filterJust,snapshot)
import           Network.Socket             (AddrInfo (..), AddrInfoFlag (..),
                                             sClose,SocketType (..), accept,
                                             bindSocket, defaultHints,
                                             defaultProtocol, getAddrInfo,
                                             listen, socket, socketToHandle,
                                             withSocketsDo)
import           Prelude                    ()
import           System.IO                  (IOMode (..))
import           Text.Printf                (printf)

data FrpSocket = FrpSocket {
    frpsDataReceived :: Event BS8.ByteString
  , frpsHandle       :: Handle
  , frpsErrorOccured :: Event SomeException
  }

forkAndThen :: (Either SomeException a -> IO ()) -> IO a -> IO ThreadId
forkAndThen = flip forkFinally

newListenSocket :: Int -> IO (Event FrpSocket)
newListenSocket p = do
  addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing
                    (Just (show p))
  case headMay addrinfos of
   Just serveraddr -> do
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     bindSocket sock (addrAddress serveraddr)
     listen sock myport
     printf "Listening on port %d\n" myport
     (eventNewSocket,pushNewSocket) <- sync newEvent
     _ <- forkAndThen (\_ -> sClose sock) (forever $ do
       (clientSocket, addr) <- accept sock
       h <- socketToHandle clientSocket ReadWriteMode
       printf "Accepted connection from: %s\n" (show addr)
       (eventDataReceived,pushDataReceived) <- sync newEvent
       (eventError,pushError) <- sync newEvent
       _ <- forkAndThen (\(Left e) -> sync $ pushError e)
                        (forever (BS8.hGetLine h >>= \l -> sync (pushDataReceived l))) -- FIXME: Das macht das ganze zeilenbasiert, man sollte hGetLine durch was geileres ersetzen
       sync $ pushNewSocket (FrpSocket eventDataReceived h eventError)
       )
     return eventNewSocket
   Nothing -> error "Couldn't get port addrinfo, exiting"

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

{-
 - Die Funktion soll zukünftig eine Statemachine implementieren:
   - Anfangs wartet sie auf 2 Spieler.
   - Wenn 2 Spieler da sind, wird das Spiel gestartet.
   - Quittet danach einer der Spieler, wird das Spiel beendet und man ist im Anfangszustand.
   - Geht das Spiel aus, passiert quasi dasselbe.
   - Wenn man im Spiel ist, werden keine neuen Spieler zugelassen.
 -}
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

{-
 - Was ist Input und Output beim reaktiven Programm?
 - Clientmap, World? 
 -}

type ClientMap = Map.Map PlayerName PerClientData

parseData :: BS8.ByteString -> ClientInput
parseData l = let line = (T.pack . BS8.unpack) l
              in if "name " `isPrefixOf` line
                 then ClientInputName (drop 5 line)
                 else ClientInputOther line
         
program :: Event FrpSocket -> Reactive (Behavior ClientMap)
program socketEvent = accum Map.empty (fmap handleSocket socketEvent)
  where handleSocket :: FrpSocket -> (ClientMap -> ClientMap)
        handleSocket s m = filterJust $ snapshot f (parseData <$> (frpsDataReceived s)) m

main :: IO ()
main = withSocketsDo $ do
  ls <- newListenSocket myport
  return ()
  {-
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
       forkFinally (clientHandler h serverChan) (\_ -> putStrLn "Closing client handle..." >> hClose h)
   Nothing -> putStrLn "Couldn't get port addrinfo, exiting"
  -}
