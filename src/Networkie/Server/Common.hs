module Networkie.Server.Common where

import qualified Data.Text                  as T
import           ClassyPrelude
import Prelude()

type PlayerName = T.Text

type World = Int

{-| Input in the "game is running stage" from a client -}
type GameClientInput = T.Text

type Client = Chan ClientCommand

{-| (Parsed) Message from the client to the server -}
-- Nachricht vom Client an den Server
data ServerCommand = SCommandNewClient PlayerName Client
                   | SCommandClientRemove PlayerName
                   | SCommandClientInput PlayerName GameClientInput

{-| Message from the server to the client -}
data ClientCommand = CCommandWorldUpdate World
                   | CCommandReject
                   | CCommandAccept PlayerName
                   deriving(Show)
