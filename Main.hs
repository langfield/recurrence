module Main where

import Test.Hspec
import Test.LeanCheck ((==>))
import qualified Test.Hspec.LeanCheck as LC

import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

f :: Double -> Double -> Double -> Double -> Double
f a b c x = x - c * (x ** a - 1) * (x ** b - 1)

g :: Double -> Double -> Double -> Double -> [Double]
g a b c = iterate (f a b c)

-- 2 - 1
-- 3 - 1/2
-- 4 - 1/3
-- 5 - 1/4

main :: IO ()
main = do
  print $ take 1000 $ g 1 1 1 2
  hspec $ do
    describe "g" $ do
      it "converges" $ do
        LC.propertyFor 1000 $ \a b c x ->
          let a' = trace' "\na" $ 1 / (abs a + 1)
              b' = trace' "b"   $ 1 / (abs b + 1)
              c' = trace' "c"   $ abs c
              x0 = trace' "x0"  $ abs x + 1
              xn = trace' "xn"  $ g a' b' c' x0 !! 10000
           in 
              x0 > 1
           && c < 1 / (x0 - 1)
           && all (>0) [a',b',c']
           && not (any isInfinite [a',b',c',x0])
             ==> abs (1 - xn) < 0.1
