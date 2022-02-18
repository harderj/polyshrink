module Demos.Demo4 where

import Lib

import Graphics.Gloss

main_ :: IO ()
main_ = play mainDisplay white 5 initWorld firstWR eventHandlerA1
  advanceWorldA1

firstWR :: WorldA -> Picture
firstWR s = shrinkedS s firstPath

shrinkedS :: Float -> Path -> Picture
shrinkedS s = Scale s' s' . stdPolygonC
  where s' = exp (s / 5)

