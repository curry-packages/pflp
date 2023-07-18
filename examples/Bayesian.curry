import PFLP

{- Wet grass example -}

flip :: Probability -> Dist Bool
flip p = enum [True,False] [p, 1 - p]

rain :: Dist Bool
rain = flip 0.2

sprinkler :: Bool -> Dist Bool
sprinkler False = flip 0.4 
sprinkler True = flip 0.01

grassWet :: Bool -> Bool -> Dist Bool
grassWet False False = flip 0.0
grassWet False True  = flip 0.8
grassWet True  False = flip 0.9
grassWet True  True  = flip 0.99

grassWetTrue :: Probability
grassWetTrue = id ?? (rain >>>= \r -> sprinkler r >>>= \s -> grassWet s r)

-- different approach using a model data type
condProb :: [a -> Bool] -> Dist a -> Probability
condProb ps dx = (\x -> all ($ x) ps) ?? dx

data GrassModel = M { r_ :: Bool, s_ :: Bool, g_ :: Bool}

grassModel :: Dist GrassModel
grassModel = rain >>>= \r -> sprinkler r >>>= \s -> grassWet s r >>>= \g -> certainly (M r s g)

grassWetWhenRain_ :: Probability
grassWetWhenRain_ = (\model -> r_ model && g_ model) ?? grassModel

grassWetTrue_ :: Probability
grassWetTrue_ = (\model -> g_ model) ?? grassModel

-- conditional probabilities
grassCondProb :: [GrassModel -> Bool] -> Probability
grassCondProb ps = condProb ps grassModel

rainWhenGrassWet :: Probability
rainWhenGrassWet = grassCondProb [g_, r_] / grassCondProb [g_]

sprinklerWhenGrassWet :: Probability
sprinklerWhenGrassWet = grassCondProb [g_, s_] / grassCondProb [g_]


{- more type-safe wet grass example -}

dmap :: (a -> b) -> Dist a -> Dist b
dmap f d = d >>>= certainly . f

data Rain = R { unR :: Bool }
data Sprinkler = S { unS :: Bool }
data Grass = G { unG :: Bool }

rainT :: Dist Rain
rainT = dmap R (flip 0.2)

sprinklerT :: Rain -> Dist Sprinkler
sprinklerT (R False) = dmap S (flip 0.4)
sprinklerT (R True)  = dmap S (flip 0.01)

grassWetT :: Sprinkler -> Rain -> Dist Grass
grassWetT (S False) (R False) = dmap G (flip 0.0)
grassWetT (S False) (R True)  = dmap G (flip 0.8)
grassWetT (S True)  (R False) = dmap G (flip 0.9)
grassWetT (S True)  (R True)  = dmap G (flip 0.99)

grassModelT :: Dist GrassModel
grassModelT = rainT >>>= \r -> sprinklerT r >>>= \s -> grassWetT s r >>>= \g -> certainly (M (unR r) (unS s) (unG g))

grassWetWhenRainT :: Probability
grassWetWhenRainT = (\model -> r_ model && g_ model) ?? grassModelT

grassWetTrueT :: Probability
grassWetTrueT = (\model -> g_ model) ?? grassModelT
