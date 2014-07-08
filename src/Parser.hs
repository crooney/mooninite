{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Parser (
      parseLisp
    ) where

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
  , fWith fScope <$> ((pSymbol "letfn" <|> pSymbol "let") *> pBindings (pElem <|> pLambda)
                      <:> pMany pElem)
  , pLambda
  , fWith fOnce <$> (pSymbol "defonce" *> pExact 2 pElem)
  , fWith fAssign <$> (pSymbol "defn" *> pIdent <:> pExact 1 (fWith fLambda <$>
      (pParams <:> pSome pElem)))
  , fWith fAssign <$> ((pSymbol "set!" <|> pSymbol "def") *> pExact 2 pElem)
  , fWith fProg <$> (pSymbol "do" *> pSome pElem)
  ]

pName :: Parser String
pName = lexeme $ pSymbol "+" <|> pSymbol "-" <|>
        ((pLetter <|> pAnySym (drop 2 legal))
        <:> pMany (pDigit <|> pLetter <|> pAnySym legal))
  where legal = "+-!$%&*/<=>?@^~._:"

pIdent, pAtom, pNumber, pString, pMultiString, pSexp, pQList, pElem
  , pParams, pVector, pLambda :: Parser Form

pIdent = fIdent <$> pName <?> "Identifier"
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
                , pVector]

pParams = pParens $ fWith fList <$> pMany pIdent <?> "Lambda parameters"

pVector = fWith fVector <$> pBrackets (pMany pElem) <?> "Vector"

pLambda = fWith fLambda <$> (pSymbol "fn" *> pParams <:> pMany pElem)

pBindings :: Parser Form -> Parser Form
pBindings f = pParens $ fWith fBindList <$> pSome go <?> "Bindings"
  where go = pParens $ fWith fAssign <$> pIdent <:> pExact 1 f <?> "Binding"

parseLisp :: String -> String -> String
parseLisp desc = concatMap gShow . runParser desc (pWhitespace *> pMany pElem)

-- utility funcs.

pWhitespace :: Parser String
pWhitespace = const "" <$> pMany (pAnySym ", \t\r\n" <|> pComment)
  <?> "Whitespace"

pComment :: Parser Char
pComment = const ' ' <$>
    ((pSymbol ";|" <* pSexp) <<|>
        (pSym ';' *> pSome (pAnySym $ filter (/= '\n') ['\000' .. '\254'])))
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
