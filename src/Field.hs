module Field where

import Data.Map(Map)
import qualified Data.Map as Map

data FieldOf s a = Field { getField :: s -> a
                         , setField :: a -> s -> s
                         }

(~>) :: FieldOf s a -> FieldOf a b -> FieldOf s b
outer ~> inner = Field { getField = getField inner . getField outer
                       , setField = \b s -> let a  = getField outer s
                                                a1 = setField inner b a
                                            in setField outer a1 s
                       }


mapField :: Ord a => a -> FieldOf (Map a b) b
mapField k = Field { getField = \s   -> s Map.! k
                   , setField = \x s -> Map.insert k x s
                   }


