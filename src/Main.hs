module Main where

import State
import GameTypes

main :: IO ()
main = debug

steps = traceState initS

initS =
    (CharId 0 ~> CardId 0) (ActivateCardOn (CharId 1))
  $ (CharId 0 ~> System) DrawTop
  $ (CharId 0 ~> CardId 0) (Card strike := 0)
  $ (CharId 0 ~> CardId 1) (Card defend := 0)
  $ (CharId 0 ~> CardId 2) (Card defend := 0)
  $ setup



setup = (System ~> CharId 1) (HP := 30)
      $ (System ~> CharId 0) (Count InDiscard := 0)
      $ (System ~> CharId 0) (Count InDraw := 0)
      $ (System ~> CharId 0) (Count InHand := 0)
      $ (System ~> CharId 0) (HP := 20)
      $ sys (Approved := 0)
      $ initState 0


strike = CardInfo { cardName = Strike, cardLocation = InDiscard }
defend = CardInfo { cardName = Defend, cardLocation = InDiscard }
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


