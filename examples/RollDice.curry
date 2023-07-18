{- Use ":set path ../src" to load the file
   if you did not install the package using CPM. -}

import PFLP

data Side = One | Two | Three | Four | Five | Six
  deriving Eq

die :: Dist Side
die = uniform [One,Two,Three,Four,Five,Six]

rollDice :: Int -> Dist [Side]
rollDice n
  | n == 0    = certainly []
  | otherwise = joinWith (:) die (rollDice (n - 1))

allSix :: Int -> Probability
allSix n = all (== Six) ?? rollDice n
