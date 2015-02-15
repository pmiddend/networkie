module Main where

import Network.Socket
import Control.Applicative
import Networkie.Terminal
import Networkie.Rect
import Control.Concurrent.MVar(MVar,putMVar,newEmptyMVar,takeMVar)
import Control.Concurrent.Chan(Chan,newChan,writeChan,readChan)
import Control.Concurrent(forkIO)
import Control.Monad(forever)

data ClientMessage = ClientCharInput Char | ClientNetworkInput String

inputThread :: Chan ClientMessage -> IO ()
inputThread mv = {-run (mkRectPosDim (0,0) (80,25)) (-}forever inputThread'{-) -}
  where inputThread' = do
          l <- getLine
          writeChan mv (ClientCharInput (head l))
          --c <- getCharEvent
          --liftTerminal (putMVar mv (ClientCharInput c))

networkThread :: Chan ClientMessage -> IO ()
networkThread mv = do
  sock <- socket AF_INET Stream 0
  let hints = (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
  addrInfo <- head <$> getAddrInfo hints (Just "localhost") (Just "31337")
  connect sock (addrAddress addrInfo)
  forever $ do
    -- Achtung: Das gibt in network-2.6 String zurück, muss man dafür fixen.
    thisBytes <- recv sock 4096
    writeChan mv (ClientNetworkInput thisBytes)

main :: IO ()
main = do 
  mv <- newChan
  _ <- forkIO (inputThread mv)
  _ <- forkIO (networkThread mv)
  forever $ do
    v <- readChan mv
    case v of
      ClientCharInput c -> putStrLn $ "char input: " ++ [c]
      ClientNetworkInput s -> putStrLn $ "network input: " ++ s


