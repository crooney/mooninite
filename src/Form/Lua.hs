{-# LANGUAGE NoMonomorphismRestriction #-}

module Form.Lua where

import           Form

fIf, fProg, fCall, fAssign, fLocal, fScope, fLambda, fQList, fBindList
  , fList :: Form

fIf = fDef { minArgs = 2 , maxArgs = 3 , gen = go }
  where go f = concat [ "ifthenelse(", arg f 0, ",thunk(", arg f 1, "), thunk( "
                      , if length (args f) > 2 then arg f 2 else "nil"
                      , "))\n"]

fProg = fDef { gen = go }
  where go f = concat [ "function()\n", intercalate "\n" $ init (args f)
                      , "\nreturn ", gShow $ last (args f), "end\n" ]

fCall = fDef { minArgs = 1, gen = go }
  where go f = concat [arg f 0 , "(", restComma f 1, ")\n"]

fAssign = fDef { minArgs = 2 , maxArgs = 2, gen = go }
  where go f = concat [arg f 0, " = ", arg f 1, "\n"]

fLocal = fAssign { gen = \f -> concat ["local ", arg f 0, " = ", arg f 1, "\n"] }

fScope = fDef { gen = \f -> concat ["do\n", arg f 0, restSemi f 1, "end\n"] }

fLambda = fDef { minArgs = 2, gen = go }
  where go f = concat [ "function", parens (arg f 0)
                      , restSemi f 1, " end\n" ]

fQList = fDef { gen = \f -> if null (args f) then " nil "
                              else "list" ++ parens (restComma f 0) }

fList = fDef { gen = (`restComma` 0) }

fBindList = fDef { gen = concatMap gShow . args }
