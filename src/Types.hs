module Types (
    Exp (..)
    ) where

import Data.Foldable

data Exp = LAtom String
         | LIdent String
         | LString String
         | LNumber Double
         | LNil
         | LBool Bool
         | LQuote Exp
         | LQuasi Exp
         | LList [Exp]
         | LComment String
  deriving (Show, Eq)
