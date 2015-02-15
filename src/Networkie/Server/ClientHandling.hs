{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Networkie.Server.ClientHandling(
    acceptLoop
  ) where

import           ClassyPrelude
import           Control.Concurrent         (forkFinally, forkIO, myThreadId,
                                             throwTo)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, put)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Text                  as T
import           Network.Socket             (Socket, accept, socketToHandle)
import           Networkie.Server.Common
import           Prelude                    ()
import           System.IO                  (IOMode (..),BufferMode(..),hSetBuffering)
import           Text.Printf                (printf)

{-| Input from the client -}
data ClientInput = ClientInputName PlayerName
                 | ClientInputOther GameClientInput
                 deriving(Show)


data ClientState = ClientState {
    csPlayerName      :: Maybe PlayerName
  , csReadChan        :: Chan (Either ClientInput ClientCommand)
  , csToServerChan    :: Chan ServerCommand
  , csClientChan      :: Chan ClientCommand
  , csSocketWriteChan :: Chan T.Text
  }

type ClientMainLoopState a = StateT ClientState IO a

clientRead :: Handle -> Chan ClientInput -> IO ()
clientRead h inputChan = forever $ do
  putStrLn $ "Starting to read..."
  line <- (T.pack . BS8.unpack) <$> BS8.hGetLine h
  --line <- (T.pack . BS8.unpack) <$> BS8.hGet h 8
  putStrLn $ "Read " ++ line
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
  putStrLn $ "Starting to write..."
  line <- readChan c
  putStrLn $ "Got a write!"
  BS8.hPut h (encodeUtf8 line)
  putStrLn $ "Write done!"

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

acceptLoop :: Socket -> Chan ServerCommand -> IO ()
acceptLoop sock serverChan = forever $ do
  (clientSocket, addr) <- accept sock
  h <- socketToHandle clientSocket ReadWriteMode
  hSetBuffering h NoBuffering
  printf "Accepted connection from: %s\n" (show addr)
  forkFinally (clientHandler h serverChan) (\_ -> putStrLn "Closing client handle..." >> hClose h)
