
module Lib where

import Util

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Cross
import Data.VectorSpace

mainDisplay :: Display
mainDisplay = InWindow "Nice Window" (800, 600) (10, 10)

stdPolygon :: Path -> Picture
stdPolygon p = Pictures [
  --color (greyN 0.9) $ Polygon p, -- not working for non-convex
  color black $ lineLoop p ]
  where center = average2 p

stdPolygonC :: Path -> Picture
stdPolygonC p = uncurry Translate (negateV center) $ stdPolygon p
  where center = average2 p

stdPolygons :: [Path] -> Picture
stdPolygons ps = Pictures $ map stdPolygon ps

type WorldA = Float
initWorld = 0 :: WorldA

--eventHandlerA1 :: Event -> WorldA -> WorldA
eventHandlerA1 (EventKey (SpecialKey KeyUp) Down mod _) = (+1)
eventHandlerA1 (EventKey (SpecialKey KeyDown) Down mod _) = (+(-1))
eventHandlerA1 (EventKey (MouseButton LeftButton) Down _ (x, y)) = const y
eventHandlerA1 _ = id

--advanceWorldA1 :: Float -> WorldA -> WorldA
advanceWorldA1 t = id

firstPath = map (`scale2` 100) $ centralize2 [
    (0, 0), (0, 3), (3, 4), (4, 0), (2.4, 0), (2.2, 1), (2.1, 0)
  ] :: Path

secondPath = map (`scale2` 100) $ centralize2 $ reverse [
    (-4, 4), (0, 0), (1, 0), (5, 4)
  ] :: Path

thirdPath = map (`scale2` 100) $ centralize2 [
    (-2, 0), (1, 1), (1, 3), (0, 3), (-0.1, 3.3), (-0.2, 3), (-1.5, 3),
    (-2, 5), (3, 5), (3, 3), (2.5, 2.5), (2.5, 1.5),
    (4, -1)
  ] :: Path
