module Main where

import Data.Bits (xor)
import Test.LeanCheck.Utils.Operators ((|||))

f :: Double -> Double -> Double -> Double -> Double
f a b c x = x - c * (x ** a - 1) * (x ** b - 1)

g :: Double -> Double -> Double -> Double -> [Double]
g a b c = iterate (f a b c)

xnor :: Bool -> Bool -> Bool
xnor u v = not (u `xor` v)

bsearch :: Fractional a => (a -> Bool) -> a -> a -> [a]
bsearch p a b
  | p a `xnor` p b = []
  | p m            = a : bsearch p a m
  | otherwise      = a : bsearch p m b
  where
    m = (a + b) / 2

diverges :: [Double] -> Bool
diverges = (isInfinite ||| isNaN) . (!! 10000)

gsearch :: Double -> Double -> (Double, Double) -> [Double]
gsearch a x0 (lb, ub) = bsearch (diverges . g') lb ub
  where g' c = g a a c x0

-- 2 - 1
-- 3 - 1/2
-- 4 - 1/3
-- 5 - 1/4

main :: IO ()
main = do
  mapM_ print $ take 50 $ g 0.5 0.5 0.2 6
