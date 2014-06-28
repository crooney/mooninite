{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Parser (
      parseLisp
    ) where

import           Prelude          hiding (lookup)
import           Data.Char
import           Data.List
-- import           Data.Map         hiding (map)
import           Text.ParserCombinators.UU.Derived
import           Text.ParserCombinators.UU.Core
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils
                   hiding (pSpaces, lexeme, pParens, pBrackets)
import qualified Text.ParserCombinators.UU.Utils as U

import           Form
import           Form.Lua

pSpecial :: Parser Form
pSpecial = pChoice
  [ fWith fIf <$> (pSymbol "if" *> pExact 3 pElem)
  , fWith fIf <$> (pSymbol "when" *> pExact 2 pElem)
  , fWith fScope <$> (pSymbol "let" *> pBindings <:> pMany pElem)
  , fWith fLambda <$> (pSymbol "lambda" *> pParams <:> pMany pElem)
  , fWith fAssign <$> ((pSymbol "set!" <|> pSymbol "define") *> pExact 2 pElem)
  , fWith fProg <$> (pSymbol "begin" *> pSome pElem)
  ]

-- Lua doesn't support any non-alphanumeric characters in identifiers
-- except '_', ':' and '.' . Scheme supports many, but is case insensitive, so
-- we downcase and then map bad characters to capital letters.
pName :: Parser String
pName = lexeme $ map (go . toLower) <$>
        (pLetter <|> pAnySym (drop 2 legalChars))
        <:> pMany (pDigit <|> pLetter <|> pAnySym legalChars)
  where go x =  case x `elemIndex` badChars of
                 Just i  -> goodChars !! i
                 Nothing -> x
        badChars   = "+-!$%&*/<=>?@^~"
        goodChars  = take (length badChars) ['A' ..]
        legalChars = badChars ++ "._:"

pIdent,pAtom,pNumber,pString,pMultiString,pSexp,pQList,pElem
  ,pParams,pBindings :: Parser Form

pIdent = fSelf <$> pName <?> "Identifier"
pAtom = fSelf <$> pSym '#' <:> pName <?> "Atom"
pNumber = (fSelf . show) <$> pDouble <?> "Number"

pString = go <$> pQuotedString <?> "Normal String"
  where go x = fSelf $ concat [ "\"", x, "\"" ]
pMultiString = go <$> pBrackets (pBrackets (pMunch (/= ']'))) <?> "Raw String"
  where go x = fSelf $ concat [ "[[", x, "]]" ]

pSexp = pParens $ pSpecial `micro` 1 <|> call `micro` 2 <?> "S-expression"
  where call = fWith fCall <$> pSome pElem

pQList = fWith fQList <$> (pSym '\'' *> pParens (pMany pElem)) <?> "Quoted List"

pElem = pChoice [ pIdent, pAtom, pNumber, pString, pMultiString, pSexp, pQList
                ]

pParams = pParens $ fWith fList <$> pMany pIdent <?> "Lambda parameters"

pBindings = pParens $ fWith fBindList <$> pSome go <?> "Bindings"
  where go = pParens $ fWith fLocal <$> pIdent <:> pExact 1 pElem <?> "Binding"

parseLisp :: String -> String -> String
parseLisp desc = concatMap gShow . runParser desc (pSpaces *> pMany pSexp)

-- utility funcs.

pSpaces :: Parser String
pSpaces = const "" <$> pMany (pAnySym ", \t\r\n")

lexeme :: ParserTrafo a a
lexeme p = p <* pSpaces

infixr 5 <:>
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> q = (:) <$> p <*> q

pParens, pBrackets :: ParserTrafo a a
pParens = lexeme . U.pParens
pBrackets = lexeme . U.pBrackets

pChoice :: IsParser p => [p a] -> p a
pChoice = foldr1 (<|>)

-- pComment :: Parser Form
-- pComment = LComment <$>
--     ((const "" <$> pToken ";|" <* pSexp)
--     <<|> (pToken ";#" *> pMunch (/= '\n'))
--     <<|> (const "" <$> (pToken ";" *> pMunch (/= '\n'))))
--     <?> "Comment"
