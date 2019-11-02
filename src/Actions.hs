module Actions where

import Types


data UpdatingAction
  = NoOp
  | Interact Label
  deriving (Eq, Show)


data Action
  = Panic
  | Look
  | LookAt Label
  | Inventory
  | Update UpdatingAction
  | Help
  | BadInput (Maybe String)
  deriving (Show)
