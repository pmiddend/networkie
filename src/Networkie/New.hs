{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module New where

import           ClassyPrelude
import           Text.Printf                (printf)
import qualified Data.ByteString.Char8 as BS8
import Control.Lens((^.),makeLenses)
import           Prelude               ()
import qualified Data.Text as T
import  Control.Concurrent(forkIO,forkFinally,ThreadId)
import Control.Monad.Loops(unfoldWhileM)
import           Network.Socket             (AddrInfo (..), AddrInfoFlag (..),sClose,Socket,
                                             SocketType (..),
                                             bindSocket, defaultHints,
                                             defaultProtocol, getAddrInfo,
                                             listen, socket, accept,socketToHandle,
                                             withSocketsDo,setSocketOption,SocketOption(..))
import           System.IO                  (IOMode (..),BufferMode(..),hSetBuffering)

data IncomingCommand a = NewClient Handle
                       | ClientClosed Handle
                       | IncomingData Handle a

data OutgoingCommand a = CloseClient Handle
                       | SendData Handle a

type Port = Int

type IncomingDataConversion a = BS8.ByteString -> Maybe (a,BS8.ByteString)
type OutgoingDataConversion a = a -> BS8.ByteString

data Server a = Server {
    _incomingChannel :: Chan (IncomingCommand a)
  , _outgoingChannel :: Chan (OutgoingCommand a)
  , _acceptThread :: ThreadId
  , _commandThread :: ThreadId
  }

$(makeLenses ''Server)

readCommand :: Server a -> IO (IncomingCommand a)
readCommand = undefined

writeCommand :: Server a -> (OutgoingCommand a) -> IO ()
writeCommand = undefined

hGetAll :: Handle -> IO BS8.ByteString
hGetAll h = BS8.concat <$> unfoldWhileM ((== 1024) . BS8.length) (BS8.hGetSome h 1024)

unfoldRest :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldRest fct ini = go fct ini mempty
  where                                               
    go f s acc =                                      
      case f s of                                     
       Nothing -> (acc, s)                           
       Just (a, b) -> go f b (acc <> [a])            

clientRead :: Handle ->
              Chan (IncomingCommand a) ->
              IncomingDataConversion a ->
              IO ()
clientRead h incomingChannel incomingConversion = clientRead' mempty
  where clientRead' buffer = do
          bytes <- hGetAll h
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
  _ <- forkFinally (clientRead h incomingChannel incomingConversion) closeHandler
  return ()

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
  killThread (s ^. acceptThread)
  killThread (s ^. commandThread)
