module Main where

import State
import GameTypes

main :: IO ()
main = see testState


testState =
  runState
  $ sendMessage 0 2 (ActivateCard 1)
  $ sendMessage 0 0 (GainAttribute HP 20)
  $ sendMessage 1 1 (GainAttribute HP 20)
  $ sendMessage 0 2 (GainAttribute (Card Strike InHand) 7)
  $ initState
