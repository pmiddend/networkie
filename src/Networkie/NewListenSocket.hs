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
