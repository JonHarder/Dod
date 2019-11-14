{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Aeson

newtype Time = Time Int
  deriving (Eq, Ord, FromJSON, FromJSONKey)

instance Show Time where
  show (Time t) = "Time left: " ++ show t

newtype Label = Label String
  deriving (Eq, Ord, FromJSONKey)

instance Show Label where
  show (Label l) = l
