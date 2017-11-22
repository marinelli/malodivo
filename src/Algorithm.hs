--------------------------------------------------------------------------------
-- | 
-- Module      : Algorithm
-- Note        : 
-- 
-- Resolve the CM, withouth normalizing on bills category
-- 
--------------------------------------------------------------------------------



module Algorithm where

import Data.List        (transpose, partition)
import Control.Arrow    ((&&&))

-- row context
-- weights are both a per-cell cap and a columnar redistribution weight
data B b  = B {
    name        :: b, -- row name
    amount      :: Int, -- row maximum cost
    weights     :: [Int] -- per column cap and weight for this row
    } 

-- a row
data R b  = R 
    {   info    :: B b  -- the row context
    ,   values  :: [Int] -- actual values
    } 

-- status of the computation
data S b  = S
    {    donerows       :: [R b]  -- done part
    ,    leftovers      :: [Int]  -- columnwise unsassigned value 
    ,    undone         :: [R b]  -- part to be done
    } 

-- algorithm context to rescale a list of values to a new sum
type Scaler  = Int -> [Int] -> [Int]   

-- correct new coming undones to be augmented by surplusses, making some of them possibly doable
correct         :: Scaler 
                -> [Int]  -- leftovers per column
                -> [R b]  -- starting row view of undones
                -> ([Int],[R b])  -- new leftovers, corrected row view of undones
correct scaler ls rs = let
    (is,vs)     = unzip $ map (info &&& values) rs -- extract infos 
    ws          = transpose . map weights $ is -- columnar weights from infos
    ts          = zipWith (+) ls . map sum . transpose $ vs -- 
    xs          = zipWith scaler ts ws -- by-column repartition of caps
    ls'         = zipWith (-) ts $ map sum xs -- leftovers
    in (ls', zipWith R is $ transpose xs) -- transpose back to row and retag with info


-- compute a done row collecting residuals
-- this operation is correct iff the values have already been capped by column
move    :: Scaler  
        -> R b  -- overpaid row
        -> ([Int], R b) -- leftovers and done row
move scaler (R i xs) = (rs', R i xs') where
    xs' = scaler (amount i) xs -- scale down to the exact sum
    rs' = zipWith (-) xs xs' -- scaling residuals


-- Nothing when there are no doable undones or a new S when something has been done
step  :: Scaler -> S b -> Maybe (S b)
step scaler (S ds ls rs) = let 
    (ds',rs') = partition f rs -- partition the set of undone in doables and undoables
    f (R i xs) = sum xs >= amount i -- doable discriminant
    (as,ds'') = unzip $ map (move scaler) ds' -- promote the doables to done collecting residuals
    (ls',rs'') = correct scaler (foldl (zipWith (+)) ls as) rs' -- correct undones with residuals
    in case ds'' of
        [] -> Nothing -- doing is over
        xs -> Just $ S (ds ++ ds'') ls' rs'' -- new S

-- list all possible steps
run :: Scaler -> S b -> [S b]
run s x = x : maybe [] (run s) (step s x)


