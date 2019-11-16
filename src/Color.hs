module Color
  (blink, bold, underline, red, black, green, yellow, blue, magenta, cyan, white)
  where

escapeCode :: Int -> String
escapeCode i = "\x1b[" ++ show i ++ "m"


resetColor :: String
resetColor = escapeCode 0


effect :: Int -> String -> String
effect i s = escapeCode i ++ s ++ resetColor


blink :: String -> String
blink = effect 5


underline :: String -> String
underline = effect 4


bold :: String -> String
bold = effect 1


red :: String -> String
red = effect 31

black :: String -> String
black = effect 30


green :: String -> String
green = effect 32


yellow :: String -> String
yellow = effect 33


blue :: String -> String
blue = effect 34


magenta :: String -> String
magenta = effect 35


cyan :: String -> String
cyan = effect 36


white :: String -> String
white = effect 37
