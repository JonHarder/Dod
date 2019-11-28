module Actions where

import Types


data UpdatingAction
  = NoOp
  | Interact Label
  | Combine Label Label
  deriving (Eq, Show)


data Action
  = Panic
  | Look
  | LookAt Label
  | Inventory
  | Tell Label String
  | Update UpdatingAction
  | Help
  | BadInput String
  deriving (Show)
