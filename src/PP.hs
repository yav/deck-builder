module PP (module PP, module P) where

import Text.PrettyPrint as P hiding ((<>))
import qualified Text.PrettyPrint as PP

infixl 6 <.>

(<.>) :: Doc -> Doc -> Doc
(<.>) = (PP.<>)

class PP a where
  pp :: a -> Doc

shPP :: PP a => a -> String
shPP = show . pp



