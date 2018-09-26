{- Use ":set path ../src" to load the file
   if you did not install the package using CPM. -}

import PFLP
import Float

{- source: https://dtai.cs.kuleuven.be/problog/tutorial/various/04_nampally.htm

It states:

   "Here’s a naive generate and test encoding whose running time quickly
    increases with the length of the string, as inference has to explicitly
    enumerate all strings of that length".
-}

pick :: Int -> Dist Char
pick _ = uniform ['a','b']

randomString :: Int -> Dist String
randomString n | n == 0 = certainly []
               | n > 0 = joinWith (:) (pick n) (randomString (n-1))

palindrome :: String -> Bool
palindrome str = str == reverse str

twoBs :: String -> Bool
twoBs bs = case bs of
               ('b':'b':_) -> True
               (_:bs') -> twoBs bs'
               [] -> False

stringIsPalindrom :: Int -> Dist (Bool,String)
stringIsPalindrom n = randomString n >>>= \str -> certainly (palindrome str, str)

stringWithBB :: Int -> Dist (Bool,String)
stringWithBB n = randomString n >>>= \str -> certainly (twoBs str, str)

testQuery1, testQuery2, testQuery3, testQuery4, testQuery5 :: Int -> Probability
testQuery1 n = (id . fst) ?? stringIsPalindrom n
testQuery2 n = (id . fst) ?? stringWithBB n

-- P (A \cap B)
testQuery3 n = id ?? (randomString n >>>= \str -> certainly (palindrome str && twoBs str))
-- P (A | B) = P (A \cap B) / P(B)
testQuery4 n = testQuery3 n /. testQuery1 n

testQuery5 n = ((\ (x,y) -> x && y) ?? q) /. ((id . fst) ?? q)
 where q = randomString n >>>= \str -> certainly (palindrome str, twoBs str)