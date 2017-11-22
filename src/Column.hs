{-# language ExistentialQuantification, Rank2Types, DeriveFunctor #-}

module Column where

import Redistribute     (capped)
import Control.Arrow    ((&&&), second)
import Data.Either      (rights, partitionEithers)
import Data.List        (transpose, sortOn)
import Data.List.OnPartition


-- a capping structure for a column
data Column a 
    = Bottom [a] -- final caps
    | Layer [(a, Column a)]  -- group caps
    deriving Show


-- 2 groups capped at 1500 and 2500 each with its own subcaps per bill
c0 :: Column Int
c0 = Layer [(1500, Bottom [400,600,800]), (2500, Bottom [700,1200,500])]

-- the tax redistributor for a set of caps
type Capper a v = v -> [a] -> [v]
{-
-- unuseful flatting a Column to a set of capped values
flattenStupid :: Capper a v -> v -> Column a -> [v]
flattenStupid f v (Bottom xs) = f v xs
flattenStupid f v (Layer ls ) =
  let (cs, bs) = unzip ls
      new_caps = f v cs
      new_ls   = zip new_caps bs
  in
    concat $ zipWith (uncurry $ flattenStupid f) (f v cs) bs
-}
-- an active column as a list of 
-- either a fixed value or 
-- a machine able to recreate the column on cell deletion

type ColumnP v a = [Either v (Cell v a)]

-- a cell has a value and a function from a value to the new column
-- the function is meant to implement row deletion
-- it accept the cell assigned value to recompute the column 
-- it's eventually a form of Moore machine
data Cell v a = Cell 
        {   proposal :: v 
        ,   acceptance :: v -> ColumnP v a
        }

unzipCells :: [Cell v a] -> ([v],[v -> ColumnP v a])
unzipCells = unzip . map (proposal &&& acceptance)

data RColumn v a 
    = RBottom [Either v a]
    | RLayer [Either (v, RColumn v v) (a, RColumn v a)]
    deriving Show

makeRColumn :: Column a -> RColumn v a
makeRColumn (Bottom ls) = RBottom $ map Right ls
makeRColumn (Layer ls) = RLayer $ map Right . map (fmap makeRColumn) $ ls

tackle :: Capper a v -> v -> RColumn v a -> RColumn v v
tackle f v (RBottom ls) = RBottom $ onRights (f v) ls
tackle f v (RLayer ls) = RLayer $ onRights g ls where
    g rs = let
        (cs, bs) = unzip rs
        in zipWith (\v b -> (v, tackle f v b)) (f v cs) bs

-- the idea here is to support for deletion in the result of flattening
-- we have to close on column values and capping structure, taking care of
-- lefts
flattenSmart ::  (v -> [a] -> [v]) -> v -> Column a -> ColumnP v a
flattenSmart = undefined
{-
flattenSmart f v c = let 
    
    actuals v c
    undefined
    flatten v c' (Bottom ls)  = let
        
        ls' = rights ls

    in flatten v (Right <$> c) (
-}
-- a solution is just the list of columns
type Solution v a = [ColumnP v a]

-- to be modeled, the idea is that we need something to decide if there is one (?)  promotable row
-- based on the row proposals. 
-- When there is such a row, the final values should be enough to
-- produce any result needed by the algo
type Discriminant v 
    =   forall r  -- any future
    .   [([v],[v] -> r)] -- rows and relative future
    ->  Maybe r -- Nothing when there is no future

-- produce all the steps until no promotion is possible
driver :: Discriminant v -> Solution v a -> [Solution v a]
driver f xs 
    = (xs:) -- produce solution
    . maybe [] (driver f) -- end production on Nothing
    . f -- let the discriminant produce the future
    -- for every row! keep rights, unzip proposal and acceptance
    -- and use acceptance on the incoming final values
    . map (second (zipWith ($)) . unzipCells . rights)   
    . transpose $ xs -- transpose to rows
