
import Algorithm (run)
import Redistribute (distribute)
import Input (Input (..), boot)
import Pretty (renderS)

b0 = Input [] [] :: Input String
b1 = Input [("ob1",9),("ob2",10),("ob3",12)]
    [ (10, [0,10, 0])
    , (9, [8, 7, 4])
    , (15, [5,12,20])
    ]
main = mapM_ (putStrLn . renderS) $
    let         cappedScaler s  = zipWith min <*> distribute s 
    in          run cappedScaler (boot cappedScaler b1)
