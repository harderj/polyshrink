module Demos.Demo3 where

import Lib

import Graphics.Gloss

main_ :: IO ()
main_ = animate mainDisplay white secondAnimation

firstPolygon :: Picture
firstPolygon = stdPolygonC secondPath

secondAnimation :: Float -> Picture
secondAnimation t = Scale s s firstPolygon
  where s = sin t
