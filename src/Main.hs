module Main where

import Text.Read(readMaybe)

import State
import Game


main :: IO ()
main = return ()


{-
round :: Action
round = Say "> "
      $ GetInput \txt ->
        case txt of
          "e" -> endPlayerTurn
          _ | Just (n:more) <- mapM readMaybe (words txt) ->
              GetState \s ->
              if 0 <= n && n < length (hand s)
                 then case more of
                        [] -> play
                 else 
          _ -> Say "Invalid input" round
-}



