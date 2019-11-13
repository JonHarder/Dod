{-# LANGUAGE OverloadedStrings #-}

module Color
  (red, black, green, yellow, blue, magenta, cyan, white)
  where

import Data.Text (Text)
import qualified Data.Text as T

resetColor :: Text
resetColor = "\x1b[0m"


red :: Text -> Text
red s = T.concat ["\x1b[31m", s, resetColor]

black :: Text -> Text
black s = T.concat ["\x1b[30m",s, resetColor]


green :: Text -> Text
green s = T.concat ["\x1b[32m", s, resetColor]


yellow :: Text -> Text
yellow s = T.concat ["\x1b[33m", s, resetColor]


blue :: Text -> Text
blue s = T.concat ["\x1b[34m", s, resetColor]


magenta :: Text -> Text
magenta s = T.concat ["\x1b[35m", s, resetColor]


cyan :: Text -> Text
cyan s = T.concat ["\x1b[36m", s, resetColor]


white :: Text -> Text
white s = T.concat ["\x1b[37m", s, resetColor]
