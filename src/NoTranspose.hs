{-# language ScopedTypeVariables #-}
import Data.List.OnPartition (onRights)
import Redistribute

through :: (a -> b -> a) -> [a] -> [b -> [a]]
through k = through' id where
    through'  f [] = []
    through'  f (x:xs) 
        = (\y -> f $ k x y : xs)
        : through' (f . (x:)) xs

type CPS c v = (c, v -> CList c v)
data CList c v = CList {
        topcap :: c
        , unCList :: [Either v (CPS c v)]
        }

data ReCapper c v = ReCapper 
    {   recap :: c -> [c] -> [c] -- modulate caps
    ,   retop :: c -> v -> c -- reduce cap
    }

recapper = ReCapper distribute (-)

cList :: forall c v .  ReCapper c v ->  c -> [c] -> CList c v
cList (ReCapper g rt) tc = f tc . map Right where
    f tc = CList tc . r where
        r ::  [Either v c] -> [Either v (CPS c v)] 
        r xs = let
            p (Right _) y = Left y
            p l _ = l
            ys = through p xs
            -- preserve 
            q g (_,d)  = (d, (f . rt tc) <*> g)
           in zipWith (fmap . q) ys $ onRights (zip <*> g tc) xs
        

actuals (CList v xs) = (v,map (fmap fst) xs)
step n (CList _ xs) = let
    Right (_,f) = xs !! n in f
