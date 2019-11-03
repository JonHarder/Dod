module Color
  (red, black, green, yellow, blue, magenta, cyan, white)
  where


resetColor :: String
resetColor = "\x1b[0m"



red :: String -> String
red s = "\x1b[31m" ++ s ++ resetColor

black :: String -> String
black s = "\x1b[30m" ++ s ++ resetColor


green :: String -> String
green s = "\x1b[32" ++ s ++ resetColor


yellow :: String -> String
yellow s = "\x1b[33" ++ s ++ resetColor


blue :: String -> String
blue s = "\x1b[34" ++ s ++ resetColor


magenta :: String -> String
magenta s = "\x1b[35" ++ s ++ resetColor


cyan :: String -> String
cyan s = "\x1b[36" ++ s ++ resetColor


white :: String -> String
white s = "\x1b[37" ++ s ++ resetColor
