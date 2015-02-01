{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.Chan
import Control.Monad(forever)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.IO.Class(liftIO)

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

type PlayerName = String

type World = Int

data ServerCommand = SCommandNewClient PlayerName (Chan ClientCommand)
                   | SCommandClientRemove PlayerName
                   | SCommandClientData ClientData

data ClientCommand = CCommandWorldUpdate World
                   | CCommandReject

data ClientData = ClientData {
  cdClientChan :: Chan ClientCommand
  }

data ServerData = ServerData {
    sdMainChannel :: Chan ServerCommand
  , sdClients :: Map.Map PlayerName ClientData
  , sdWorld :: World
  }

type MainLoopState a = StateT ServerData IO a

mainLoop :: MainLoopState ()
mainLoop = forever $ do
  state <- get
  command <- liftIO $ readChan (sdMainChannel state)
  case command of
   SCommandNewClient pn clientchan -> do
     if pn `Map.member` (sdClients state)
       then do liftIO $ putStrLn "Client already exists, rejecting"
               liftIO $ writeChan clientchan CCommandReject
       else do liftIO $ putStrLn "Client is new, inserting"
               put (state { sdClients = Map.insert pn (ClientData clientchan) (sdClients state) })
               liftIO $ writeChan clientchan (CCommandWorldUpdate (sdWorld state))

myport :: Int
myport = 31337

main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral myport))
  printf "Listening on port %d\n" myport
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)
