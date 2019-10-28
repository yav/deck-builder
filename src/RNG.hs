{-# LANGUAGE Trustworthy, BlockArguments #-}
module RNG
  ( Gen
  , genRand, genRand_, genRandFun
  , genRandIO, genRandIO_
  , RNG, newRNG, withNewRNG, seededRNG

  , shuffle
  , oneOf
  , randInt
  , randInRange
  , randIdent
  , randRNG
  ) where

import qualified System.Random.TF as Rand
import qualified System.Random.TF.Gen as Rand
import qualified System.Random.TF.Instances as Rand
import           Data.Text (Text)
import qualified Data.Text as Text
import Data.Array (listArray, bounds, (!))
import Data.Array.ST
import Control.Monad(ap,liftM,replicateM,foldM_)
import Control.Monad.ST(ST, runST)

newtype RNG = RNG Rand.TFGen

instance Show RNG where
  show _ = "RNG"

newRNG :: IO RNG
newRNG = RNG <$> Rand.newTFGen

withNewRNG :: (RNG -> a) -> IO a
withNewRNG k = k <$> newRNG

seededRNG :: Int -> RNG
seededRNG = RNG . Rand.mkTFGen


genRand :: RNG -> Gen a -> (a, RNG)
genRand g m = genRandFun g do a <- m
                              pure \r -> (a,r)

genRand_ :: RNG -> Gen a -> a
genRand_ g m = fst (genRand g m)

genRandIO  :: Gen a -> IO (a, RNG)
genRandIO m = withNewRNG (`genRand` m)

genRandIO_ :: Gen a -> IO a
genRandIO_ m = withNewRNG (`genRand_` m)


genRandFun :: RNG -> Gen (RNG -> a) -> a
genRandFun (RNG g) (Gen m) = let (f,g1) = m g
                                in f (RNG g1)


newtype Gen a = Gen (Rand.TFGen -> (a,Rand.TFGen))

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
  pure a = Gen \g -> (a,g)
  (<*>)  = ap

instance Monad Gen where
  Gen m >>= k = Gen \g -> let (a,g1) = m g
                              Gen m1 = k a
                           in m1 g1

-- | Get a new randomness source.
randRNG :: Gen RNG
randRNG = Gen (\g -> let (g1,g2) = Rand.split g
                        in (RNG g1, g2))

-- | Shuffle the elements of the given input
shuffle :: [a] -> Gen [a]
shuffle xs = shuffleM <$> randRNG
  where
  len = length xs

  shuffleM g0 = runST
    do x <- newListArrayST len xs
       let l = len - 1
           newLoc g i = do let (j,g1) = genRand g (randInRange i l)
                           swap x i j
                           pure g1
       foldM_ newLoc g0 [ 0 .. len - 2 ]
       getElems x

newListArrayST :: Int -> [a] -> ST s (STArray s Int a)
newListArrayST len = newListArray (0, len - 1)

swap :: STArray s Int a -> Int -> Int -> ST s ()
swap x a b =
  do t <- readArray x a
     writeArray x a =<< readArray x b
     writeArray x b t

-- | Get one of the given elemnts at random.
oneOf :: [a] -> Gen a
oneOf xs = do let (l,h) = bounds as
              n <- randInRange l h
              return (as ! n)
  where
  as = listArray (0, length xs - 1) xs

-- | Get an inte in the given range (inclusive).
randInRange :: Int -> Int -> Gen Int
randInRange l h = Gen (\g -> Rand.randomR (l,h) g)

-- | Get a random int.
randInt :: Gen Int
randInt = Gen Rand.random

-- | Keep running the random generator, until it generates
-- something that satisfies the predicate.  Note that this
-- process is not guaranteed to stop.
randomTill :: (a -> Bool) -> Gen a -> Gen a
randomTill p g =
  do a <- g
     if p a then return a else randomTill p g

-- | Generate a random piece of text, of the given length,
-- only containing characters satisfying the given predicate.
randIdent :: Int {- ^ How long -} ->
            (Text -> Bool) {- ^ Is it used -} ->
            Gen Text
randIdent n used = randomTill (not . used) (Text.pack <$> replicateM n randChar)
  where
  randChar = toEnum <$> randInRange 65 90

