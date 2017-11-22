{-# language ViewPatterns #-}

module Input where

import Data.List (transpose)

import Algorithm (S (..), R (..), B (..), Scaler)

-- initial data model
data Input b = Input 
    {   rows            :: [(b,Int)] -- name + cost per row
    ,   columns         :: [(Int, [Int])] -- weights + starting sum per column
    }

-- compute first solution (no success) from the input data
boot    :: Scaler
        -> Input b
        -> S b
boot scaler (Input kis cs@(unzip -> (ts,ws)))  = S [] ds rs where
    rs = zipWith R ks $ transpose xs -- zip contexts to values
    ds = zipWith (-) ts $ map sum xs -- compute the leftovers
    xs = map (uncurry scaler) cs -- prepare the (transposed) value starting table
    ks = zipWith (uncurry B) kis . transpose $ ws -- make row contexts


