{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Networkie.Server.ServerFramework(
    IncomingCommand(..)
  , OutgoingCommand(..)
  , Port
  , IncomingDataConversion
  , OutgoingDataConversion
  , Server
  , readCommand
  , writeCommand
  , runServer
  , closeServer
  ) where

import           ClassyPrelude
import           Control.Concurrent    (ThreadId, forkFinally, forkIO,
                                        killThread)
import           Control.Lens          (makeLenses, (^.))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T
import           Network.Socket        (AddrInfo (..), AddrInfoFlag (..),
                                        Socket, SocketOption (..),
                                        SocketType (..), accept, bindSocket,
                                        defaultHints, defaultProtocol,
                                        getAddrInfo, listen, sClose,
                                        setSocketOption, socket, socketToHandle,
                                        withSocketsDo)
import           Networkie.List        (unfoldRest)
import           Prelude               ()
import           System.IO             (BufferMode (..), IOMode (..),
                                        hSetBuffering)
import           Text.Printf           (printf)

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
   CloseClient h -> putStrLn "Closing handle" >> hClose h
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
