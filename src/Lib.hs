module Lib
    ( someFunc
    , radixsort
    , radixsortIteration
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

radixsort :: Integral a => [a] -> [a]
radixsort [] = []
radixsort [x] = [x]
radixsort xs = radixsortIteration maxDivisor 1 [[], []] xs
  where maxDivisor = floor $ logBase 2 $ fromIntegral $ maximum xs

radixsortIteration :: Integral a => a -> a -> [[a]] -> [a] -> [a]
radixsortIteration maxDivisor divisor [evens, odds] []
  | divisor > maxDivisor + 1 = evens ++ odds
  | otherwise                = radixsortIteration maxDivisor (divisor * 2) [[], []] (evens ++ odds)
radixsortIteration maxDivisor divisor [evens, odds] (x:xs)
  | even $ div x divisor = radixsortIteration maxDivisor divisor [evens ++ [x], odds] xs
  | otherwise            = radixsortIteration maxDivisor divisor [evens, odds ++ [x]] xs
