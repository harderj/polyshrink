module Demos.Demo5 where

import Lib
import Util

import Graphics.Gloss

main_ :: IO ()
main_ = play mainDisplay white 5 initWorld worldRenderer eventHandlerA1
  advanceWorldA1

worldRenderer :: WorldA -> Picture
worldRenderer s = shrinkedA s firstPath

shrinkedA :: Float -> Path -> Picture
shrinkedA t = stdPolygonC . shrinkA t

shrinkA :: Float -> Path -> Path
shrinkA s p = let
  n = length p
  pt = (p !!) -- point from index
  ptnl i = pt $ (i - 1 + n) `mod` n -- left neighbour
  ptnr i = pt $ (i + 1 + n) `mod` n -- right neighbour
  cp i = crossProd2 (pt i - ptnl i) (ptnr i - pt i)
  ptr i = ((ptnl i `sub2` pt i) `add2` (ptnr i `sub2` pt i)) `scale2` cp i
  ptrn = normalize2 . ptr
  ptn i = pt i `add2` ptrn i `scale2` (s * 15)
  in [ptn i | i <- [0..(n-1)]]
