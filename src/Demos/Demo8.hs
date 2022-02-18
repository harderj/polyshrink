module Demos.Demo8 where

import Lib
import Util

import Graphics.Gloss
    ( white, play, Path, Picture(Circle, Pictures, Translate) )

main_ :: IO ()
main_ = play mainDisplay white 5 initWorld worldRenderer eventHandlerA1
  advanceWorldA1

worldRenderer :: WorldA -> Picture
worldRenderer s = shrinkC s secondPath

shrinkC :: Float -> Path -> Picture
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
  cols = concatMap (\(pi, ai, bi) -> let
      a0 = pt ai `sub2` pt pi 
      b0 = pt bi `sub2` pt pi 
      va = vd ai `sub2` vd pi
      vb = vd bi `sub2` vd pi
      ptc t = pt pi `add2` (vd pi `scale2` t)
      in
        map (\t -> (pi, ai, bi, t, ptc t)) $ lineSegCol a0 b0 va vb
    ) $ triples n
  in Pictures [
      stdPolygon [ptn i | i <- [0..(n - 1)]],
      Pictures $ map (\p -> uncurry Translate (ffth5 p) $ Circle 10) cols
    ]
