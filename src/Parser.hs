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
                   hiding (lexeme, pParens, pBrackets)

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
parseLisp desc = concatMap gShow . runParser desc (pWhitespace *> pMany pSexp)

-- utility funcs.

pWhitespace :: Parser String
pWhitespace = const "" <$> pMany (pAnySym ", \t\r\n" <|> pComment)
  <?> "Whitespace"

pComment :: Parser Char
pComment = const ' ' <$>
    ((pSymbol ";|" <* pSexp) <<|>
        (pSym ';' *> (pSome $ pAnySym $ filter (/= '\n') ['\000' .. '\254'])))
    <?> "Comment"

lexeme :: ParserTrafo a a
lexeme p = p <* pWhitespace

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
p <:> q = (:) <$> p <*> q

pParens, pBrackets :: ParserTrafo a a
pParens = lexeme . pPacked pLParen pRParen
pBrackets = lexeme . pPacked pLBracket pRBracket

pChoice :: IsParser p => [p a] -> p a
pChoice = pAny id
