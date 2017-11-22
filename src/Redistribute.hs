module Redistribute (distribute, capped) where

import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Tuple (swap)

tsort f = map snd . sortBy (f . comparing $ fst)

someOnes s = replicate s 1 ++ repeat 0

-- | distribute an integral quantity given some weights
distribute :: Int -> [Int] -> [Int]
distribute s xs
  | all (== 0) xs = xs
  | otherwise 
      = tsort id
      -- redistribute with residuals
      . (\(n,xs) -> zipWith (fmap . (+)) (someOnes n) xs) 
      -- count residuals and reorder list by descending fractionals
      . ((s -) . sum . map snd &&& tsort flip . zipWith (fmap . (,)) [0..])
      -- fractional scaling to tagged by fractional part
      . map (\x -> swap . properFraction $ fromIntegral (x * s) / fromIntegral (sum xs)) 
      $ xs

capped :: Int -> [Int] -> [Int]
capped s = zipWith min <*> distribute s
