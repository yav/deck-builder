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


main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     rng <- newRNG
     s0  <- runAction_ newGame (newState rng)
     play s0


newGame :: Action ()
newGame =
  do forM_ [ 1 .. 12 ] \i -> addTo theDiscarded =<< newCard (noOp i)
     update player (updateAttribute Health 60)
     startPlayerTurn

play :: State -> IO ()
play s =
  do putStrLn ""
     print (pp s)
     putStr "> "
     cmd <- getLine
     case words cmd of
       ["e"] -> play =<< runAction_ endPlayerTurn s
       w : more | Just c <- parseChoice (getField theHand s) w ->
         case more of
           [] -> play =<< runAction_ (playCardFromHand c NoTaget) s
           [we] | Just e <- parseChoice (enemies s) we ->
                 play =<< runAction_ (playCardFromHand c (Target e)) s
           _ -> again
       _ -> again

  where
  again = do putStrLn "Invalid command"
             play s


runAction_ :: Action () -> State -> IO State
runAction_ act s = snd <$> runScript (doAction act s)

runAction :: Action a -> State -> IO (a,State)
runAction act s = runScript (doAction act s)

runScript :: Script a -> IO (a,State)
runScript scr =
  case scr of
    Done a s1 -> pure (a,s1)
    Choose s msg cnt cs k ->
      do ds <- getInp s msg cnt cs
         runScript (k ds)

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



