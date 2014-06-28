{-# LANGUAGE NoMonomorphismRestriction #-}

module Form where

import qualified Data.List as L

data Form = Form { minArgs  :: Int
                 , maxArgs  :: Int
                 , validate :: Form -> Bool
                 , orig     :: Form -> String
                 , gen      :: Form -> String
                 , args     :: [Form]
                 }

gShow,oShow :: Form -> String
gShow f = gen f f
oShow f = orig f f

arg :: Form -> Int -> String
arg f i = gen a a
  where a = args f !! i

intercalate :: String -> [Form] -> String
intercalate s = L.intercalate s . map gShow

rest :: String -> Form -> Int -> String
rest s f i = intercalate s $ drop i $ args f

restComma, restSpace, restSemi, restLine :: Form -> Int -> String
restComma = rest ","
restSpace = rest " "
restSemi  = rest ";"
restLine  = rest "\n"

fWith :: Form -> [Form] -> Form
fWith f xs = f {args = xs}

fWithS :: Form -> [String] -> Form
fWithS f = fWith f . map fSelf

fDef :: Form
fDef = Form { minArgs  = 0
            , maxArgs  = 1000
            , args     = []
            , validate = val
            , gen      = undefined
            , orig     = undefined
            }
  where val f = (minArgs f <= length (args f)) && (length (args f) <= maxArgs f)

fSelf :: String -> Form
fSelf s = fDef {gen = const s, orig = const s}

surround :: String -> String -> String -> String
surround l r s = concat [l, s, r]

parens,brackets,braces,quotes :: String -> String
parens   = surround "(" ")"
brackets = surround "[" "]"
braces   = surround "{" "}"
quotes   = surround "\"" "\""
