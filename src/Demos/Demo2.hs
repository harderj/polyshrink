module Demos.Demo2 where

import Lib

import Graphics.Gloss

main_ :: IO ()
main_ = animate mainDisplay white firstAnimation

firstAnimation :: Float -> Picture
firstAnimation t = Color red $ Circle $ 100 * sin t ^ 2
