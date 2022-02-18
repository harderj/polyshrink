module Demos.Demo1 where

import Lib

import Graphics.Gloss

main_ = display mainDisplay white firstPicture :: IO ()

firstPicture = Color black $ Circle 100 :: Picture