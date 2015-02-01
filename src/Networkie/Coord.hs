module Networkie.Coord where

type Coord = (Int, Int)
type Dim = (Int, Int)

pairPlus :: Num a => (a,a) -> (a,a) -> (a,a)
pairPlus (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

pairMinus :: Num a => (a,a) -> (a,a) -> (a,a)
pairMinus (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)

pairDiv :: Integral a => a -> (a,a) -> (a,a)
pairDiv n (a1, a2) = (div a1 n, div a2 n)

pairNegate :: Num a => (a,a) -> (a,a)
pairNegate (a1,a2) = (-a1,-a2)

cog :: [Coord] -> Coord
cog xs = (pairDiv (length xs)) (foldl1 pairPlus xs)
