module Pretty where

import Algorithm (S (..), R (..), B (..))
import Text.PrettyPrint

prettyR (R (B b a ws) xs) = (text b <> parens (int a))
    $$ nest 9 (vcat (zipWith nest [3,6..] $ map int xs ))
    $$ nest (9 + 3 * (length xs + 1)) (vcat (zipWith nest [3,6..] $ map int ws))

prettyS (S ds ps ws) =  
    text ""
    $$ vcat (map (\x -> text "* " <> prettyR x) ds)
    $$ vcat (map (\x -> text "- " <> prettyR x) ws)
    $$ text "leftovers" $$ nest 11 (vcat (zipWith nest [3,6..] $ map int ps))

renderS = render . prettyS

