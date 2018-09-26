{-# OPTIONS_CYMAKE -W no-missing-signatures #-}
import SecretSanta
import PFLP
kics2MainGoal = query 3 (\d -> pickRound d >>>= certainly . normFailedGame)
