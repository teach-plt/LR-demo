module DebugPrint where

class DebugPrint a where
  debugPrint :: a -> String
