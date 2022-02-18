{-# LANGUAGE FlexibleContexts #-}

module Util where

import Data.VectorSpace
import Graphics.Gloss

-- gen

headMb :: [a] -> Maybe a
headMb l
  | null l = Nothing
  | otherwise = Just $ head l

frst5 :: (a, b, c, d, e) -> a
frst5 (a, _, _, _, _) = a

frth5 :: (a, b, c, d, e) -> d
frth5 (_, _, _, d, _) = d

ffth5 :: (a, b, c, d, e) -> e
ffth5 (_, _, _, _, e) = e

triples :: Int -> [(Int, Int, Int)]
triples n = [
    (p, a, b) |
    p <- [0..(n - 1)],
    a <- [0..(n - 1)],
    let b = (a + 1) `mod` n,
    a /= (p - 1 + n) `mod` n,
    b /= (p + 1 + n) `mod` n]

-- math

sec :: Floating a => a -> a
sec = (1 / ) . cos

scale2 :: Num b => (b, b) -> b -> (b, b)
scale2 (x, y) s = (x * s, y * s)

add2 :: Num a => (a, a) -> (a, a) -> (a, a)
add2 (x, y) (z, w) = (x + z, y + w)

sub2 :: Num a => (a, a) -> (a, a) -> (a, a)
sub2 (x, y) (z, w) = (x - z, y - w)

dot2 :: Num a => (a, a) -> (a, a) -> a
dot2 (x, y) (z, w) = x * z + y * w

norm2 :: Floating a => (a, a) -> a
norm2 (x, y) = sqrt $ x ^ 2 + y ^ 2

normalize2 :: Floating a => (a, a) -> (a, a)
normalize2 v = v `scale2` (1 / norm2 v)

rot90 :: Num a => (a, a) -> (a, a)
rot90 (x, y) = (y, -x)

crossProd2 :: Num a => (a, a) -> (a, a) -> a
-- crossProd2 (x, y) (z, w) = x * w - y * z
crossProd2 a b = a `dot2` rot90 b

sum2 :: Num a => [(a, a)] -> (a, a)
sum2 = foldr add2 (0, 0)

average2 :: Fractional b => [(b, b)] -> (b, b)
average2 vs = sum2 vs `scale2` (1 / fromIntegral (length vs))

centralize2 :: Fractional a => [(a, a)] -> [(a, a)]
centralize2 vs = map (`sub2` average2 vs) vs

rootsDeg2 :: (Ord a, Floating a) => a -> a -> a -> [a]
rootsDeg2 a b c
  | d < 0 = []
  | d == 0 = [x]
  | otherwise = [x, y]
  where
    x = e + sqrt d / (2 * a)
    y = e - sqrt d / (2 * a)
    d = b ^ 2 - 4 * a * c
    e = - b / (2 * a)

-- line segment collision time
lineSegCol :: (Ord a, Floating a) => (a, a) -> (a, a) -> (a, a) -> (a, a) -> [a]
lineSegCol a0 b0 va vb = let
  ca = va `crossProd2` vb
  cb = (a0 `crossProd2` vb) - (b0 `crossProd2` va)
  cc = a0 `crossProd2` b0
  pts = rootsDeg2 ca cb cc
  a t = a0 `add2` (va `scale2` t)
  b t = b0 `add2` (vb `scale2` t)
  in filter (\t -> (a t `dot2` b t) <= 0.01 && t >= -0.01) pts

-- trash

{-

averageV :: (VectorSpace a, Fractional (Scalar a)) => [a] -> a
averageV [] = undefined -- todo
averageV vs = sumV vs ^/ fromIntegral (length vs)

centralize :: (VectorSpace b, Fractional (Scalar b)) => [b] -> [b]
centralize p = map (`sub2` c) p where c = averageV p

uglyFunction :: Floating a => (a, a) -> (a, a) -> (a, a) -> (a, a) -> a
uglyFunction (a_0x, a_0y) (b_0x, b_0y) (v_ax, v_ay) (v_bx, v_by)
  = 1/2*(b_0y*v_ax - b_0x*v_ay - a_0y*v_bx + a_0x*v_by
  + sqrt(b_0y^2*v_ax^2 - 2*b_0x*b_0y*v_ax*v_ay + b_0x^2*v_ay^2 + a_0y^2*v_bx^2
  + a_0x^2*v_by^2 - 2*(a_0y*b_0y*v_ax + (a_0y*b_0x - 2*a_0x*b_0y)*v_ay)*v_bx
  - 2*(a_0x*b_0x*v_ay + a_0x*a_0y*v_bx - (2*a_0y*b_0x - a_0x*b_0y)*v_ax)*v_by))
  /(v_ay*v_bx - v_ax*v_by)

cross :: (InnerSpace a, Num a) => (a, a) -> (a, a) -> Scalar a
cross v u = v <.> rot90 u

-}

