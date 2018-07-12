--- Library for Probabilistic Functional Logic Programming
--- @author Sandra Dylus, Jan Christiansen, Finn Teegen, Jan Tikovsky
--- @version June 2018

module PFLP
  ( Probability
  , Dist
  , enum
  , uniform
  , certainly
  , (>>>=)
  , joinWith
  , (??)
  , RT
  , pick
  , replicateDist
  , partitionDist
  , S (..)
  , SDist
  , sample
  , sampleDist
  ) where

import List         (partition, sum)
import Findall      (allValues)
import FiniteMap    (addListToFM_C, emptyFM, fmToList)

import Random

infixl 1 >>>=
infixr 1 ??

--- Probabilities.
--- Floating point numbers are used to model probabilities.
type Probability = Float

--- Probability distributions.
--- Distributions are abstract and can only be created using the functions
--- provided by this library, e.g., 'enum' and 'uniform'. Internally, Curry's
--- built-in non-determinism is used to model distributions with more than one
--- event-probability pair.
--- Since events can be filtered by applying predicates, a flag indicates
--- whether an event is valid.
data Dist a = Dist { event :: a, prob :: Probability, valid :: Bool }
  deriving Show

member :: [a] -> a
member = foldr (?) failed

--- Creates a distribution based on a given list of events and another list
--- providing the corresponding probabilities. This function also ensures that
--- the relevant probabilities add up to `1.0` and are strictly positive.
enum :: [a] -> [Probability] -> Dist a
enum xs ps
  | 1.0 - sum ps' < eps && all (> 0.0) ps'
  = member (zipWith3 Dist xs ps' (repeat True))
  | otherwise
  = error ("PFLP.enum: probabilities do not add up to 1.0 " ++
             "or are not strictly positive")
 where ps' = take (length xs) ps
       eps = 0.0001

--- Creates a uniform distribution based on a given list of events. The list
--- of events must be non-empty.
uniform :: [a] -> Dist a
uniform [] = error "PFLP.uniform: list of events must be non-empty"
uniform xs@(_:_) = enum xs (repeat (1.0 / fromInt (length xs)))

--- Creates a single-event-distribution with probability `1.0`.
certainly :: a -> Dist a
certainly x = Dist x 1.0 True

--- Combines two (dependent) distributions.
(>>>=) :: Dist a -> (a -> Dist b) -> Dist b
d >>>= f = let Dist x p v1 = d
               Dist y q v2 = f x
           in Dist y (p * q) (v1 && v2)

--- Combines two (independent) distributions with respect to a given function.
joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f d1 d2 = do
  x <- d1
  y <- d2
  return (f x y)

--- Partition a distribution by applying a given predicate to its events
partitionDist :: (a -> Bool) -> Dist a -> Dist a
partitionDist pred (Dist x p v) = Dist x p (v && pred x)

--- Queries a distribution for the probability of events that satisfy a given
--- predicate.
(??) :: (a -> Bool) -> Dist a -> Probability
(??) p = sum . allValues . prob . filterDist p
  where filterDist pred d | pred (event d) = d

--- Run-time choice values. Currently, the only way to construct a run-time
--- choice value is to explicitly use a lambda abstraction. The evaluation of
--- a run-time choice can be triggered by the function 'pick'.
type RT a = () -> a

--- Triggers the evaluation of a run-time choice value (see type synonym 'RT').
--- Everytime a run-time choice value is evaluated, a new choice is made.
pick :: RT a -> a
pick rt = rt ()

--- Independently replicates a distribution a given number of times. In order
--- to behave properly, the given distribution is required to be a run-time
--- choice value (see type synonym 'RT').
replicateDist :: Int -> RT (Dist a) -> Dist [a]
replicateDist n rt
  | n == 0    = certainly []
  | otherwise = joinWith (:) (pick rt) (replicateDist (n - 1) rt)

instance Monad Dist where
  return = certainly
  (>>=)  = (>>>=)

-- Sampling of events and distributions

type Seed = StdGen

-- sampled event
data S a = S { runS :: Seed -> (a, Seed) }

instance Monad S where
  return x = S $ \s -> (x, s)
  
  m >>= f = S $ \s -> let (x, s') = runS m s
                      in runS (f x) s'

--- Create a sampled event
sample :: (a -> Dist b) -> a -> S b
sample f = choose . f

choose ::  Dist a -> S a
choose d = S $ \s -> let (p, s') = randomP s
                     in (scanP p (filter valid (allValues d)), s')

scanP :: Probability -> [Dist a] -> a
scanP p ((Dist x q _):ds)
  | p <= q || null ds = x
  | otherwise         = scanP (p - q) ds
scanP _ []            = error "PFLP.scanP: Distribution must be non-empty"

rescale :: [Dist a] -> [Dist a]
rescale ds = map (\(Dist x p v) -> Dist x (rv + p) v) vs
  where
  (vs, ivs) = partition valid ds
  rv        = sum (map prob ivs) / fromInt (length vs)

--- sampled distribution
type SDist a = S [(a, Probability)]

toSDist :: Ord a => [S a] -> SDist a
toSDist xs = sequence xs >>= return . norm . map (\x -> (x, 1.0 / fromInt len))
  where len = length xs

norm :: Ord a => [(a, Probability)] -> [(a, Probability)]
norm = fmToList . addListToFM_C (+) (emptyFM (<))

--- Create a sampled distribution for a given probalistic function
sampleDist :: Ord b => Int -> (a -> Dist b) -> a -> SDist b
sampleDist n f = toSDist . replicate n . sample f
