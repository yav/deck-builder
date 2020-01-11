module Attributes
  ( Attrs
  , EntAttr(..)
  , CardAttr(..)
  , noAttributes
  , hasAttribute
  , removeAttribute
  , attrEndOfRound
  , attribute
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Field
import PP

newtype Attrs k = Attrs (Map k Int)

class Ord a => Attr a where
  removeAtZero :: a -> Bool
  singleTurn   :: a -> Bool


--------------------------------------------------------------------------------
data EntAttr = MaxHealth | Health | Block | Vulnerable | Strength
             | MaxEnergy | Energy
  deriving (Show,Eq,Ord)


fromSet :: Ord a => [a] -> a -> Bool
fromSet xs = (`Set.member` Set.fromList xs)

instance Attr EntAttr where
  removeAtZero = fromSet [ Block, Vulnerable ]
  singleTurn   = fromSet [ Vulnerable ]


--------------------------------------------------------------------------------
data CardAttr = TurnCost
  deriving (Show,Eq,Ord)

instance Attr CardAttr where
  removeAtZero = fromSet []
  singleTurn   = fromSet [TurnCost]



--------------------------------------------------------------------------------

-- | No attributes.
noAttributes :: Attrs a
noAttributes = Attrs Map.empty

-- | Check if we have an attribute.
hasAttribute :: Attr a => a -> Attrs a -> Bool
hasAttribute a (Attrs mp) = Map.member a mp

-- | Remove the given attribute.
removeAttribute :: Attr a => a -> Attrs a -> Attrs a
removeAttribute a (Attrs mp) = Attrs (Map.delete a mp)



-- | Missing attriubtes are reported as 0.
-- Use `hasAttribute` if the presence matters.
attribute :: Attr a => a -> FieldOf (Attrs a) Int
attribute e = Field { getField = doGet
                    , setField = \x s ->
                                    case setAttr e x of
                                      Nothing -> removeAttribute e s
                                      Just v  -> doSet v s
                    }
  where
  doGet (Attrs mp) = Map.findWithDefault 0 e mp
  doSet v (Attrs mp) = Attrs (Map.insert e v mp)

-- | Decrement single turn attributes
attrEndOfRound :: Attr a => Attrs a -> Attrs a
attrEndOfRound (Attrs mp) = Attrs (Map.mapMaybeWithKey upd mp)
  where upd a v = if singleTurn a then setAttr a (v-1) else Just v

setAttr :: Attr a => a -> Int -> Maybe Int
setAttr a newVal = if newVal == 0 && removeAtZero a
                      then Nothing
                      else Just newVal

--------------------------------------------------------------------------------

instance PP EntAttr where
  pp = text . show

instance PP a => PP (Attrs a) where
  pp (Attrs xs) = vcat (map ppV (Map.toList xs))
    where ppV (a,v) = pp a <.> colon <+> pp v


