module Demos.Demo7 where

import Lib
import Util

import Graphics.Gloss

main_ :: IO ()
main_ = play mainDisplay white 5 initWorld worldRenderer eventHandlerA1
  advanceWorldA1

worldRenderer :: WorldA -> Picture
worldRenderer s = shrinkedC s firstPath

shrinkedC :: Float -> Path -> Picture
shrinkedC t = stdPolygons . shrinkC t

shrinkC :: Float -> Path -> [Path]
shrinkC s p = let
  n = length p
  pt = (p !!) -- point from index
  ptl i = pt $ (i - 1 + n) `mod` n -- neighbour point left
  ptr i = pt $ (i + 1 + n) `mod` n -- neighbour point right
  vl i = normalize2 (ptl i `sub2` pt i) -- vector to left neighbour normalize2
  vr i = normalize2 (ptr i `sub2` pt i) -- vector to right neighbour normalize2
  cp i = crossProd2 (vl i) (vr i) -- = sin (angle between vl, vr)
  vd i = (vl i `add2` vr i) `scale2` (1 / cp i) -- velocity vector
  ptn i = pt i `add2` (vd i `scale2` (s * 10))
  in [[ptn i | i <- [0..(n - 1)]]]
