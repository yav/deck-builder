module Main where

import Text.Read(readMaybe)
import System.IO
import Control.Monad

import RNG
import PP
import Field
import State
import Game
import Cards
import Enemies

-- import qualified Server


main :: IO ()
main = mainTerminal

mainTerminal :: IO ()
mainTerminal =
  do hSetBuffering stdout NoBuffering
     rng <- newRNG
     s0  <- runAction newGame (newState rng)
     play s0


newGame :: Action ()
newGame =
  do let addCard x = addTo theDrawPile =<< newCard x
     replicateM_ 5 (addCard strike)
     replicateM_ 5 (addCard defend)
     addCard bash
     addCard spotWeakness
     addCard reaper
     shuffleDraw

     let attr x y = set (entity player ~> entityAttrs ~> attribute x) y

     attr MaxHealth 60
     attr Health    60
     attr MaxEnergy  3

     _ <- newEnemy boss

     startPlayerTurn

play :: State -> IO ()
play s =
  do putStrLn ""
     print (pp s)
     putStr "> "
     cmd <- getLine
     case words cmd of
       ["e"] -> play =<< runAction endPlayerTurn s
       w : more | Just c <- parseChoice (getField theHand s) w ->
         case more of
           [] -> play =<< runAction (playCardFromHand c player) s
           [we] | Just e <- parseChoice (getField enemies s) we ->
                 play =<< runAction (playCardFromHand c e) s
           _ -> again
       _ -> again

  where
  again = do putStrLn "Invalid command"
             play s


runAction :: Action () -> State -> IO State
runAction act s = runScript s (doAction act s)

runScript :: State -> Script -> IO State
runScript s0 scr =
  case scr of
    Abort -> pure s0
    Done s1 -> pure s1
    Choose s msg cnt cs k ->
      do ds <- getInp s msg cnt cs
         runScript s0 (k ds)

parseChoice :: [a] -> String -> Maybe a
parseChoice cs txt =
  do c <- readMaybe txt
     guard (c > 0)
     case splitAt (c-1) cs of
       (_,b:_) -> Just b
       _       -> Nothing



getInp :: State -> String -> Count -> [Card] -> IO [Card]
getInp s msg cnt cs = go
  where
  go = do print (text msg $$ nest 2 (numbered (map (ppCard s) cs)))
          putStr "> "
          txt <- getLine
          case mapM (parseChoice cs) (words txt) of
            Just chosen ->
              do let l = length chosen
                 case cnt of
                   UpTo n
                     | l <= n -> pure chosen
                     | otherwise ->
                       do putStrLn ("You can choose no more than "
                                                ++ show n ++ " cards.")
                          go

                   Exactly n
                     | l == n -> pure chosen
                     | otherwise ->
                       do putStrLn ("You must choose exactly "
                                            ++ show n ++ " cards.")
                          go

            Nothing -> do putStrLn "Failed to parse choise."
                          go



