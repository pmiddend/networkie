module Networkie.Terminal(
    run
  , TerminalMonad
  , SneakyColor(..)
  , SneakyColorPair
  , mkColorPair
  , Tile(..)
  , render
  , getCharEvent
  , drawStringCentered
  , liftTerminal
  ) where

import           Control.Monad              (forM_)
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,gets)
import           Control.Monad.Trans.Class  (lift)
import           Data.List                  ((\\))
import           Data.Map.Strict            (Map, insert, keysSet,(!))
import           Data.Monoid                (mempty)
import           Data.Set                   (difference, fromList,toList)
import           Networkie.Coord
import           Networkie.Rect
import qualified UI.NCurses                 as C
import Control.Monad.IO.Class(liftIO)

type TerminalMonad = StateT TerminalData C.Curses

liftTerminal :: IO a -> TerminalMonad a
liftTerminal a = liftIO a

data SneakyColor = Red
                 | Green
                 | Yellow
                 | Cyan
                 | Blue
                 | White
                 | Transparent
                 deriving(Eq,Show,Ord)

toCurses :: SneakyColor -> C.Color
toCurses Red = C.ColorRed
toCurses Green = C.ColorGreen
toCurses Blue = C.ColorBlue
toCurses White = C.ColorWhite
toCurses Transparent = C.ColorDefault
toCurses Yellow = C.ColorYellow
toCurses Cyan = C.ColorCyan

data SneakyColorPair = SneakyColorPair {
    cpForeground :: SneakyColor
  , cpBackground :: SneakyColor
  } deriving(Eq,Show,Ord)

mkColorPair :: SneakyColor -> SneakyColor -> SneakyColorPair
mkColorPair = SneakyColorPair

data Tile = Tile {
    tPosition  :: !Coord
  , tCharacter :: !Char
  , tColor     :: SneakyColorPair
  } deriving(Eq)

type ColorMap = Map SneakyColorPair C.ColorID

data TerminalData = TerminalData {
    rdWindow      :: C.Window
  , rdViewport    :: Rect
  , rdPrevTiles   :: [Tile]
  , rdColors      :: ColorMap
  , rdNextColorId :: Int
  }

moveCursor :: Coord -> C.Update ()
moveCursor (x,y) = C.moveCursor (fromIntegral y) (fromIntegral x)

drawTile :: ColorMap -> Tile -> C.Update ()
drawTile colors t = do
  moveCursor (tPosition t)
  C.setColor (colors ! tColor t)
  C.drawString [tCharacter t]

clearTile :: Tile -> C.Update ()
clearTile t = do
  moveCursor (tPosition t)
  C.drawString " "

tileDiff :: ColorMap -> [Tile] -> [Tile] -> C.Update ()
tileDiff cs before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ (drawTile cs) toAdd

render :: [Tile] -> TerminalMonad ()
render ts = do
  currentColors <- gets rdColors
  let unassignedColors = fromList (map tColor ts) `difference` keysSet currentColors
  forM_ (toList unassignedColors) $ \c -> do
    nextColor <- gets rdNextColorId
    cid <- lift $ C.newColorID (toCurses (cpForeground c)) (toCurses (cpBackground c)) (fromIntegral nextColor)
    modify (\s -> s {rdNextColorId = rdNextColorId s + 1, rdColors = insert c cid (rdColors s)})
  rd' <- get
  lift $ C.updateWindow (rdWindow rd') $ tileDiff (rdColors rd') (rdPrevTiles rd') ts
  lift C.render
  modify (\s -> s {rdPrevTiles = ts})

getCharEvent :: TerminalMonad Char
getCharEvent = do
  rd <- get
  e <- lift $ C.getEvent (rdWindow rd) Nothing
  case e of
    Just (C.EventCharacter c) -> return c
    _ -> getCharEvent

clearScreen :: [Tile] -> C.Update ()
clearScreen = mapM_ clearTile

drawStringCentered :: String -> TerminalMonad ()
drawStringCentered s = do
  rd <- get
  lift $ C.updateWindow (rdWindow rd) (clearScreen (rdPrevTiles rd) >> moveCursor (((\x -> x - length s).(`div`2). fst . rDim . rdViewport) rd,((`div`2). snd . rDim . rdViewport) rd) >> C.drawString s) >> C.render

run :: Rect -> TerminalMonad a -> IO a
run standardViewport a = C.runCurses $ do
  C.setEcho False
  w <- C.newWindow ((fromIntegral . snd . rDim) standardViewport) ((fromIntegral . fst . rDim) standardViewport) 0 0
  _ <- C.setCursorMode C.CursorInvisible
  evalStateT a (TerminalData w standardViewport mempty mempty 1)
