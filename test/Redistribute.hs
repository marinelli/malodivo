{-# language ViewPatterns #-}
module Main where


import Test.QuickCheck
import Test.QuickCheck.Property (withMaxSuccess)
import "malodivo" Redistribute
import Test.QuickCheck.Modifiers (NonEmptyList (..))

tsum :: Positive Int -> NonEmptyList (Positive Int) -> Bool
tsum (Positive s) (NonEmpty (map getPositive -> xs)) 
    = sum (distribute s xs) == s


genR = do 
    Positive s <- arbitrary
    NonEmpty (map getPositive -> xs) <- arbitrary
    
    return $ zipWith (/)  (map fromIntegral xs)
         $ (map fromIntegral $ distribute s xs)

propor ::  NonEmptyList (Positive Int) -> Bool
propor  (NonEmpty (map getPositive -> xs)) = let
    s = sum xs * 10
    in 
    all id  
    $ zipWith (==) (repeat 10) 
    $ zipWith (/) 
        (map fromIntegral $ distribute s xs) 
        (map fromIntegral xs) 

main = do
    quickCheck $ withMaxSuccess 1000 tsum
    quickCheck $ withMaxSuccess 1000 propor


