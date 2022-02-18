module Demos.Demo9 where

import Lib
import Util

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe

main_ :: IO ()
main_ = play mainDisplay white 30 initWorld worldRenderer eventHandlerA2
  advanceWorldA2

eventHandlerA2 (EventKey (SpecialKey KeyUp) Down mod _) = (+50)
eventHandlerA2 (EventKey (SpecialKey KeyDown) Down mod _) = (+(-50))
eventHandlerA2 (EventKey (MouseButton LeftButton) Down _ (x, y)) = const y
eventHandlerA2 _ = id

advanceWorldA2 t = (+1)

worldRenderer :: WorldA -> Picture
worldRenderer t = shrinkC t thirdPath

shrinkC :: Float -> Path -> Picture
shrinkC t p
  | length p < 3 = Blank
  | otherwise = let
  n = length p
  pt = (p !!) -- point from index
  ptl i = pt $ (i - 1 + n) `mod` n -- neighbour point left
  ptr i = pt $ (i + 1 + n) `mod` n -- neighbour point right
  vl i = normalize2 (ptl i `sub2` pt i) -- vector to left neighbour normalize2
  vr i = normalize2 (ptr i `sub2` pt i) -- vector to right neighbour normalize2
  cp i = crossProd2 (vl i) (vr i) -- = sin (angle between vl, vr)
  vd i = (vl i `add2` vr i) `scale2` (1 / cp i) -- velocity vector
  cols = concatMap (\(pi, ai, bi) -> let -- collisions
      a0 = pt ai `sub2` pt pi
      b0 = pt bi `sub2` pt pi
      va = vd ai `sub2` vd pi
      vb = vd bi `sub2` vd pi
      ptc t = pt pi `add2` (vd pi `scale2` t)
      in map (\t -> (pi, ai, bi, t, ptc t)) $ lineSegCol a0 b0 va vb
    ) $ triples n 
  srtd = sortBy (\a b -> frth5 a `compare` frth5 b) cols -- sorted collisions
  (fpi, fai, fbi, ft, fcp) = head srtd -- first collision (if such)
  ptt t i = pt i `add2` (vd i `scale2` t) -- new points at time t
  in if null cols || t < ft -- if no collisions for before first collision
    then stdPolygon $ map (ptt t) [0..(n-1)] -- draw stdPolygon
    else let  -- recursively split polygon
      newPolyL = splitPolyL fpi fai n
      newPolyR = splitPolyR fpi fai n
      in Pictures [
        shrinkC (t - ft) $ map (ptt ft) newPolyL,
        shrinkC (t - ft) $ map (ptt ft) newPolyR
      ]

splitPolyL pi ai n = let bi = (ai + 1) `mod` n in
  ai : map (`mod` n) (takeWhile ((/= ai) . (`mod` n)) [pi..])

splitPolyR pi ai n = let bi = (ai + 1) `mod` n in
  pi : map (`mod` n) (takeWhile ((/= pi) . (`mod` n)) [bi..])
