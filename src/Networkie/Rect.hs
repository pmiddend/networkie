module Networkie.Rect where

import Networkie.Coord

data Rect = Rect {
    rTopLeft :: Coord
  , rDim :: Coord
  }

rBottomRight :: Rect -> Coord
rBottomRight (Rect p d) = p `pairPlus` d

mkRectPosDim :: Coord -> Dim -> Rect
mkRectPosDim = Rect

mkRectFromCorners :: Coord -> Coord -> Rect
mkRectFromCorners p1 p2 = Rect p1 (p2 `pairMinus` p1)

insideRect :: Rect -> Coord -> Bool
insideRect (Rect (tlx,tly) (w,h)) (x,y) = x >= tlx && y >= tly && x < (tlx+w) && y < (tly+h)
