module PP (module PP, module P) where

import Text.PrettyPrint as P hiding ((<>))
import qualified Text.PrettyPrint as PP

infixl 6 <.>

(<.>) :: Doc -> Doc -> Doc
(<.>) = (PP.<>)

class PP a where
  pp :: a -> Doc

instance PP Int where
  pp = text . show

shPP :: PP a => a -> String
shPP = show . pp

numbered :: [Doc] -> Doc
numbered ds = vcat (zipWith sh [ 1 :: Int .. ] ds)
  where
  maxW   = length (show (length ds))
  sh i d = let j = show i
           in text (replicate (maxW - length j) ' ' ++ j) <.> "." <+> d




