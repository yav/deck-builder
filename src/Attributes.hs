module Attributes
  ( Attributes
  , Attribute(..)
  , noAttributes
  , hasAttribute
  , getAttribute
  , updateAttribute
  , reduceNonNeg
  , removeAttribute
  , attrEndOfRound
  ) where

import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import PP

newtype Attributes = Attrs (Map Attribute Int)


data Attribute = Health | Block | Vulnerable
  deriving (Show,Eq,Ord)


removeAtZero :: Attribute -> Bool
removeAtZero = \a -> a `Set.member` as
  where
  as = Set.fromList
            [ Block, Vulnerable ]

singleTurn :: Attribute -> Bool
singleTurn = \a -> a `Set.member` as
  where
  as = Set.fromList
        [ Vulnerable ]



--------------------------------------------------------------------------------

-- | No attributes.
noAttributes :: Attributes
noAttributes = Attrs Map.empty

-- | Check if we have an attribute.
hasAttribute :: Attribute -> Attributes -> Bool
hasAttribute a (Attrs mp) = Map.member a mp

-- | Get the value of an attribute.  Defaults to 0 if missing.
getAttribute :: Attribute -> Attributes -> Int
getAttribute a (Attrs mp) = Map.findWithDefault 0 a mp

-- | Change the value of an attribute by this mucth,
updateAttribute :: Attribute -> Int -> Attributes -> Attributes
updateAttribute a n (Attrs mp) = Attrs (Map.alter upd a mp)
  where upd mb = updAttr a (fromMaybe 0 mb) n

reduceNonNeg :: Attribute -> Int -> Attributes -> (Int,Attributes)
reduceNonNeg a i as =
  let as1  = updateAttribute a (negate i) as
      newV = getAttribute a as1
  in if newV < 0
        then (i+newV, updateAttribute a (negate newV) as1)
        else (i,as1)

-- | Remove the given attribute.
removeAttribute :: Attribute -> Attributes -> Attributes
removeAttribute a (Attrs mp) = Attrs (Map.delete a mp)

-- | Decrement single turn attributes
attrEndOfRound :: Attributes -> Attributes
attrEndOfRound (Attrs mp) = Attrs (Map.mapMaybeWithKey upd mp)
  where upd a v = if singleTurn a then updAttr a v (-1) else Just v

updAttr :: Attribute -> Int -> Int -> Maybe Int
updAttr a x d =
  let newVal = x + d
  in if newVal == 0 && removeAtZero a
       then Nothing
       else Just newVal

--------------------------------------------------------------------------------

instance PP Attribute where
  pp = text . show

instance PP Attributes where
  pp (Attrs xs) = vcat (map ppV (Map.toList xs))
    where ppV (a,v) = pp a <.> colon <+> pp v


