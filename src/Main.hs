module Main where

import State
import GameTypes

main :: IO ()
main = debug

steps = traceState initS

initS =
    (CharId 0 ~> CharId 0) ShuffleDiscard
  $ (CharId 0 ~> CardId 0) (ActivateCard (CharId 1))
  $ (System ~> CharId 1) (GainAttribute HP 20)
  $ (System ~> CharId 0) (GainAttribute HP 20)
  $ (CharId 0 ~> CardId 0) (GainAttribute (Card Strike InHand) 7)
  $ sys      (GainAttribute Approved 0)
  $ initState

x ~> y = sendMessage x y
sys    = sendMessage System System


debug :: IO ()
debug = go 0 [] initS
  where
  go num prev cur =
    do putStrLn ("Step " ++ show num)
       print cur
       c <- getLine
       case c of
         'p' : _ | (p : more) <- prev -> go (num - 1) more p
         'q' : _ -> pure ()
         _ -> case stepState cur of
                Just s1 -> go (num + 1) (cur : prev) s1
                Nothing -> go num prev cur


