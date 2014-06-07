{-# LANGUAGE NoMonomorphismRestriction #-}

module Form where

import           Data.List

data Form = Form { minArgs :: Int
                 , maxArgs :: Int
                 , validate :: Form -> Bool
                 , fShow :: Form -> String
                 , args :: [Form]
                 }

instance Show Form where
    show f = fShow f f

arg :: Form -> Int -> String
arg f i = fShow a a
  where a = args f !! i

rest :: String -> Form -> Int -> String
rest s f i = intercalate s $ map (arg f) [i .. (length (args f) - 1)]

restComma, restSpace, restSemi :: Form -> Int -> String
restComma = rest ","
restSpace = rest " "
restSemi  = rest ";"

fWith :: Form -> [Form] -> Form
fWith f xs = f {args = xs}

fWithS :: Form -> [String] -> Form
fWithS f = fWith f . map fSelf

fDef :: Form
fDef = Form { minArgs  = 0
            , maxArgs  = 1000
            , args     = []
            , validate = val
            , fShow    = undefined
            }
  where val f = (minArgs f <= length (args f)) && (length (args f) <= maxArgs f)

fSelf :: String -> Form
fSelf s = fDef {fShow = const s}

fIf, fCall, fAssign, fLocal, fScope, fLambda, fQList :: Form
fIf = fDef { minArgs  = 3
           , maxArgs  = 3
           , fShow = \f -> concat [ "if ", arg f 0, " then \n", arg f 1
                                  , "\nelse ", arg f 2," end\n"]
           }

fCall = fDef { minArgs  = 1
             , fShow = \f -> concat [arg f 0 , "(", restComma f 1, ")\n"]
             }

fAssign = fDef { minArgs  = 2
               , maxArgs  = 2
               , fShow = \f -> concat [arg f 0, " = ", arg f 1, "\n"]
               }
fLocal = fAssign { fShow =
                     \f -> concat ["local", arg f 0, " = ", arg f 1, "\n"] }

fScope = fDef { fShow = \f -> concat ["do\n", restSpace f 0, "end\n"] }

fLambda = fDef { minArgs = 2
               , fShow = \f -> concat ["function ", arg f 0, parens (arg f 1)
                                      , restSemi f 2, " end\n"]
               }

fQList = fDef { fShow = \f -> if null (args f) then " nil "
                              else "Runtime.list" ++ parens (restComma f 0) }

surround :: String -> String -> String -> String
surround l r s = concat [l, s, r]

parens,brackets,braces,quotes :: String -> String
parens   = surround "(" ")"
brackets = surround "[" "]"
braces   = surround "{" "}"
quotes   = surround "\"" "\""

-- main = do print $ fIf {args = [fSelf "foo", fSelf "bar", fSelf "baz"]}
--           print $ fCall {args = map fSelf ["push","3","3","44","44","5"]}
--           print $ fWith fScope [(fWithS fAssign ["x","100"]),(fWithS fCall ["pop"])]
