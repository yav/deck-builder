module Color where

import Graphics.Vty.Attributes
import Data.Word

newtype Col = Col Word8

color :: Col -> Color
color (Col n) = ISOColor n

basicColors :: [Col]
basicColors = map Col [ 0 .. 7 ]

otherCol :: Col -> Col
otherCol (Col n)
  | n > 7 = Col (n - 8)
  | otherwise = Col (n + 8)

fgFor :: Col -> Col
fgFor (Col n)
  | n < 7 || n == 8  || n == 9= Col 15
  | otherwise       = Col 0

selFor :: Col -> Col
selFor (Col n)
  | n < 8     = Col (n + 8)
  | otherwise = Col (n - 8)

selAttr :: Col -> Attr
selAttr c = defAttr `withBackColor` color c
                    `withForeColor` color (fgFor c)
                    `withStyle`     underline

