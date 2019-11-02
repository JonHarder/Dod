module Types where


newtype Time = Time Int
  deriving (Eq, Ord)

instance Show Time where
  show (Time t) = "Time left: " ++ show t

newtype Label = Label String
  deriving (Eq, Ord)

instance Show Label where
  show (Label l) = l
