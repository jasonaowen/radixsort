module Lib
    ( someFunc
    , radixsort
    , radixsortIteration
    , appendToListAtIndex
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

radixsort :: Integral a => a -> [a] -> [a]
radixsort _ [] = []
radixsort _ [x] = [x]
radixsort base xs = radixsortIteration base maxDivisor 1 buckets xs
  where maxDivisor = floor $ logBase (fromIntegral base) $ fromIntegral $ maximum xs
        buckets = take (fromIntegral base) $ repeat []

radixsortIteration :: Integral a => a -> a -> a -> [[a]] -> [a] -> [a]
radixsortIteration base maxDivisor divisor buckets []
  | divisor > maxDivisor + 1 = foldl (++) [] buckets
  | otherwise                = radixsortIteration base maxDivisor (divisor * base) base_buckets (foldl (++) [] buckets)
  where base_buckets = take (fromIntegral base) $ repeat []
radixsortIteration base maxDivisor divisor buckets (x:xs)
  = radixsortIteration base maxDivisor divisor (appendToListAtIndex buckets bucket x) xs
  where bucket = rem (div x divisor) base

appendToListAtIndex :: Integral a => [[a]] -> a -> a -> [[a]]
appendToListAtIndex (list:lists) 0 value
  = (list ++ [value]):lists
appendToListAtIndex (list:lists) index value
  = list:(appendToListAtIndex lists (index - 1) value)
