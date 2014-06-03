module Types (
      Terminal (..)
    , Sexp (..)
    ) where

data Sexp = Term Terminal | Cons [Sexp]
  deriving (Show, Eq)

data Terminal = LAtom String
         | LIdent String
         | LString String
         | LNumber Double
         | LNil
         | LBool Bool
         | LQuote Terminal
         | LQuasi Terminal
         | LComment String
  deriving (Show, Eq)

