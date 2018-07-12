module Random where

external data StdGen

newStdGen :: IO StdGen
newStdGen external

split :: StdGen -> (StdGen, StdGen)
split external

randomP :: StdGen -> (Float, StdGen)
randomP external
