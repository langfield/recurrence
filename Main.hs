module Main where

import Data.Bits (xor)
import Test.LeanCheck.Utils.Operators ((|||))

import Test.Hspec
import qualified Test.Hspec.LeanCheck as LC

import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

-- | Recurrence.
f :: Double -> Double -> Double -> Double -> Double
f a b c x = x - c * (x ** a - 1) * (x ** b - 1)

-- | List of all values of recurrence starting from x0.
g :: Double -> Double -> Double -> Double -> [Double]
g a b c = iterate (f a b c)

-- | Nth value of recurrence.
h :: Double -> Double -> Double -> Double -> Int -> Double
h a b c x n = g a b c x !! n

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

lhs :: Double -> Double -> Double -> Double
lhs a b x = (x**a - 1) * (x**b - 1)

rhs :: Double -> Double -> Double
rhs d x = (x - 1)**d

main :: IO ()
main = do
  mapM_ print $ take 50 $ g 0.5 0.5 0.2 6
  hspec $ do
    describe "thing" $ do
      it "has a lower bound" $
        LC.property $ \a b ->
          let a' = trace' "a" $ 1 / (abs a + 1.0000001)
              b' = trace' "b" $ 1 / (abs b + 1.0000001)
           in LC.exists 100000 $ \d -> LC.holds 100 $ \x ->
             let x' = abs x + 1.0000001
              in lhs a' b' x' >= rhs d x'
