{-# LANGUAGE NoMonomorphismRestriction #-}

module Form.Lua where

import           Prelude hiding (lookup)
import           Data.Map (fromList, lookup)
import           Data.List (elemIndex)
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)

import           Form

fIf, fProg, fCall, fAssign, fLocal, fScope, fLambda, fQList, fBindList
  , fList, fVector, fOnce :: Form

fIf = fDef { minArgs = 2 , maxArgs = 3 , gen = go }
  where go f = concat [ "ifthenelse(", arg f 0, thunk (arg f 1)
                      , thunk $ if length (args f) > 2 then arg f 2 else "nil"
                      , ")\n"]
        thunk = (", thunk(function() return " ++) . (++ " end) ")

fProg = fDef { gen = \f -> parens (function (fSelf "") (args f)) ++ "()\n" }

fCall = fDef { minArgs = 1, gen = go }
  where go f = concat [ arg f 0 , "(", restComma f 1, ")\n"]

fAssign = fDef { minArgs = 2 , maxArgs = 2, gen = go }
  where go f = concat [arg f 0, " = ", arg f 1, "\n"]

fLocal = fAssign { gen = \f -> concat ["local ", arg f 0, " = ", arg f 1, "\n"] }

fOnce = fAssign { gen = \f -> concat  [arg f 0, " = ", arg f 0, " or ", arg f 1
                                     , "\n"] }

fScope = fDef { gen = \f -> concat ["do\n", arg f 0, restSemi f 1, "end\n"] }

fLambda = fDef { minArgs = 2,
                 gen = \f -> function (head $ args f) $ tail (args f) }

fQList = fColl "list"

fVector = fColl "vector"

fList = fDef { gen = (`restComma` 0) }

fBindList = fDef { gen = go  }
  where go f = concat [ "local ", intercalate "," $ map (head . args) (args f)
                      , "\n" , (concatMap gShow . args) f ]

fColl :: String -> Form
fColl s = fDef { gen = \f -> if null (args f) then " nil "
                             else s ++ parens (restComma f 0) }

-- Lua doesn't support any non-alphanumeric characters in identifiers
-- except '_', ':' and '.' . Scheme supports many, but is case insensitive, so
-- we downcase and then map bad characters to capitals.j
fIdent :: String -> Form
fIdent s = (fSelf s) { gen = const $ mangle s }
  where mangle x = fromMaybe (map (go . toLower) x) (lookup x ops)
        go x = case x `elemIndex` bad of
                 Just i  -> good !! i
                 Nothing -> x
        bad   = "+-!$%&*/<=>?@^~"
        good  = take (length bad) ['A' ..]
        ops = fromList [ ("+","addi"), ("-","subt"), ("*","mult")
                       , ("/","divi"), ("%","modu"), ("^","expo") ]

function :: Form -> [Form] -> String
function bs es = concat [ "function", parens $ gShow bs
                        , intercalate "\n" $ init (es)
                        , "\nreturn ", gShow $ last es, "end\n" ]
