module Main where

import Conjure
import Numeric.Natural
import Test.LeanCheck (Listable, tiers, (==>))
import Test.LeanCheck.Instances ()
import qualified Test.Hspec.LeanCheck as LC
import Debug.Trace (trace)

trace' :: Show a => String -> a -> a
trace' s x = trace (s ++ ": " ++ show x) x

newtype X = X Double deriving (Eq, Ord, Show)
newtype AB = AB Double deriving (Eq, Ord, Show)
newtype Positive = Positive Double deriving (Eq, Ord, Show)

instance Listable Positive where
  tiers = (map . map) (Positive . (+0.000001) . abs) (tiers :: [[Double]])

instance Listable AB where
  tiers = (map . map) (AB . (1/) . (+1) . (\(Positive x) -> x)) (tiers :: [[Positive]])

instance Listable X where
  tiers = (map . map) (X . (+1) . (\(Positive x) -> x)) (tiers :: [[Positive]])

-- | Recurrence.
f :: Double -> Double -> Double -> Double -> Double
f a b c x = x - c * (x ** a - 1) * (x ** b - 1)

-- | List of all values of recurrence starting from x0.
g :: Double -> Double -> Double -> Double -> [Double]
g a b c = iterate (f a b c)

-- | Nth value of recurrence.
xn :: Double -> Double -> Double -> Double -> Int -> Double
xn a b c x n = g a b c x !! n

isFinite :: Double -> Bool
isFinite x = not (isNaN x) && not (isInfinite x)

lowerBound :: (Double -> Double -> Double -> Double -> Int -> Double) -> AB -> AB -> Positive -> X -> Natural -> Bool
lowerBound h a b c x n = isFinite c' && isFinite x' ==> xn a' b' c' x' n' <= h a' b' c' x' n'
  where
    a' = (\(AB u) -> u) a
    b' = (\(AB u) -> u) b
    c' = (\(Positive u) -> u) c
    x' = (\(X u) -> u) x
    n' = fromEnum n

upperBound :: (Double -> Double -> Double -> Double -> Int -> Double) -> AB -> AB -> Positive -> X -> Natural -> Bool
upperBound h a b c x n = isFinite c' && isFinite x' ==> h a' b' c' x' n' <= 2 * xn a' b' c' x' n'
  where
    a' = (\(AB u) -> u) a
    b' = (\(AB u) -> u) b
    c' = (\(Positive u) -> u) c
    x' = (\(X u) -> u) x
    n' = fromEnum n

spec :: (Double -> Double -> Double -> Double -> Int -> Double) -> Bool
spec h = and
  [ LC.holds 2000 $ lowerBound h
  , LC.holds 2000 $ upperBound h
  , True
  , True
  ]

primitives :: [Prim]
primitives =
  [ pr (0::Double)
  , pr (1::Double)
  , pr (2::Double)
  , prim "+" ((+) :: Double -> Double -> Double)
  , prim "*" ((*) :: Double -> Double -> Double)
  , prim "-" ((-) :: Double -> Double -> Double)
  , prim "/" ((/) :: Double -> Double -> Double)
  , prim "(-)" (negate :: Double -> Double)
  , prim "**" ((**) :: Double -> Double -> Double)
  ]

closedForm x y z x' y'  =  x'

main :: IO ()
main = do
  LC.checkFor 2000 $ lowerBound closedForm
  LC.checkFor 2000 $ upperBound closedForm
  conjureFromSpecWith args{maxSize=18} "closedForm" spec primitives
