{- Use ":set path ../src" to load the file
   if you did not install the package using CPM. -}

import PFLP
import Data.List (delete)

{- source: https://youtu.be/5kC5k5QBqcc
   Thanks to @sebfisch for the idea -}

type Person = Int
type Hat = [Person]

data SecretSanta = FailedGame | Success [SantaArrangement]
data SantaArrangement = Arrangement { santa :: Person, person :: Person }

santaGame :: Int -> Hat
santaGame n | n > 1 = [1..n]
            | otherwise = error "invalid game"

pickFromHat :: Hat -> Dist Person
pickFromHat = uniform

-- yields an arrangement and the remaining persons if there are any persons to pick from;
-- the remaining persons are all given person without the one that was picked
pPicks :: Person -> Hat -> Dist (Maybe (SantaArrangement, [Person]))
pPicks _ [] = certainly Nothing
pPicks p ps@(_:_) =
  pickFromHat ps >>>= \p' ->
  certainly (Just (Arrangement p p', delete p' ps))

-- yields an arrangement for each player in the hat;
-- the pick cannot fail as there are as many players as persons in the hat
pickRound :: Hat -> Dist SecretSanta
pickRound [] = certainly FailedGame
pickRound xs@(_:_) = pickRound' xs xs [] >>>= \ arrs -> certainly (Success arrs)
 where
  pickRound' []     _   arrs = certainly arrs
  pickRound' (p:ps) hat arrs =
    pPicks p hat >>>= \ (Just (arr,hat')) ->
    pickRound' ps hat' (arr:arrs)

-- yields an arrangement for each player in the hat;
-- for each player the hat is "cleaned", such that a player
-- cannot pick herself from the hat
pickRoundWOFailed :: Hat -> Dist SecretSanta
pickRoundWOFailed [] = certainly FailedGame
pickRoundWOFailed xs@(_:_) = pickRound' xs xs []
 where
  pickRound' []     _   arrs = certainly (Success arrs)
  pickRound' (p:ps) hat arrs =
    pPicks p (delete p hat) >>>= \ mArr ->
    maybe (certainly FailedGame)
          (\(arr,_) -> pickRound' ps (delete (person arr) hat) (arr:arrs))
          mArr

-- yields a `SecretSanta` game without invalid picks, i.e.,
-- arrangements with invalid picks are transformed to `FailedGame`
normFailedGame :: SecretSanta -> SecretSanta
normFailedGame FailedGame = FailedGame
normFailedGame game@(Success arrs) | any isFailedArr arrs = FailedGame
                                   | otherwise            = game

-- checks if a game is a `FailedGame`
isFailedGame :: SecretSanta -> Bool
isFailedGame game = case game of
                      FailedGame -> True
                      _ -> False

-- checks if an arrangement is invalid
isFailedArr :: SantaArrangement -> Bool
isFailedArr secret = santa secret == person secret


santa1 :: Dist SecretSanta
santa1 = pickRound (santaGame 3)
{-
Evaluating expression: santa1
(Dist (Success [(Arrangement 3 3),(Arrangement 2 2),(Arrangement 1 1)]) 0.16666666666666666)
(Dist (Success [(Arrangement 3 2),(Arrangement 2 3),(Arrangement 1 1)]) 0.16666666666666666)
(Dist (Success [(Arrangement 3 3),(Arrangement 2 1),(Arrangement 1 2)]) 0.16666666666666666)
(Dist (Success [(Arrangement 3 1),(Arrangement 2 3),(Arrangement 1 2)]) 0.16666666666666666)
(Dist (Success [(Arrangement 3 2),(Arrangement 2 1),(Arrangement 1 3)]) 0.16666666666666666)
(Dist (Success [(Arrangement 3 1),(Arrangement 2 2),(Arrangement 1 3)]) 0.16666666666666666)
-}

santa1' :: Dist SecretSanta
santa1' = pickRound (santaGame 3) >>>= \game -> certainly (normFailedGame game)
{-
Evaluating expression: santa1'
(Dist FailedGame 0.16666666666666666)
(Dist FailedGame 0.16666666666666666)
(Dist FailedGame 0.16666666666666666)
(Dist (Success [(Arrangement 3 1),(Arrangement 2 3),(Arrangement 1 2)]) 0.16666666666666666)
(Dist (Success [(Arrangement 3 2),(Arrangement 2 1),(Arrangement 1 3)]) 0.16666666666666666)
(Dist FailedGame 0.16666666666666666)
-}

santa2 :: Dist SecretSanta
santa2 = pickRoundWOFailed (santaGame 3)
{-
Evaluating expression: santa2
(Dist FailedGame 0.25)
(Dist (Success [(Arrangement 3 1),(Arrangement 2 3),(Arrangement 1 2)]) 0.25)
(Dist (Success [(Arrangement 3 2),(Arrangement 2 1),(Arrangement 1 3)]) 0.5)
-}

query :: Int -> (Hat -> Dist SecretSanta) -> Probability
query n pickFunc = isFailedGame ?? (pickFunc (santaGame n))
{-
Evaluating expression: query 3 pickRoundWOFailed
0.25

Evaluating expression: query 3 pickRound
0.0

Evaluating expression: query 3 (\d -> pickRound d >>>= certainly . normFailedGame)
0.6666666666666666
-}
