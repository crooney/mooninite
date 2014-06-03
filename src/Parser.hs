{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts, RankNTypes #-}
module Parser (
      parseLisp
    ) where

import           Text.ParserCombinators.UU.Derived
import           Text.ParserCombinators.UU.Core
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils hiding (pSpaces, lexeme)

import           Types

(<++>) :: Parser String -> Parser String -> Parser String
p <++> q = (++) <$> p <*> q

liftL :: ParserTrafo a [a]
liftL = ((:[]) <$>)

badChars :: String
badChars = ":$%&*+/<=>@\\^|-~"

cleanChars :: String -> String
cleanChars = concatMap go
  where go x
          | x `elem` badChars = "_"
          | x == '!'          = "_d"
          | x == '?'          = "_p"
          | otherwise         = [x]

pName :: Parser String
pName = lexeme $ cleanChars <$> (liftL (pLetter <|> pAnySym "*")
        <++> pMany (pDigit <|> pLetter <|> pAnySym (".!?" ++ badChars)))

pIdent :: Parser Terminal
pIdent = go <$> pName <?> "Identifier"
  where go "nil" = LNil
        go x     = LIdent x

pAtom :: Parser Terminal
pAtom = go <$> (liftL (pSym '#') <++> pName) <?> "Atom"
  where go "#f" = LBool False
        go x    = LAtom x

pNumber :: Parser Terminal
pNumber = LNumber <$> pDouble <?> "Number"

pString :: Parser Terminal
pString = LString <$> pQuotedString <?> "Normal String"

pMultiLine :: Parser String
pMultiLine = pBrackets (pBrackets (pMunch (/= ']')))

pRawString :: Parser Terminal
pRawString = LString <$> pMultiLine <?> "Raw String"

pTerm :: Parser Sexp
pTerm = lexeme $ foldr1 (<|>) (map (Term <$>) [pAtom, pIdent, pNumber, pString,
                                               pRawString, pComment])

pSexp :: Parser Sexp
pSexp = pParens (Cons <$> pList (pTerm <|> pSexp <|> pQuote))
    <?> "S-expression"

pQuote :: Parser Sexp
pQuote = lexeme $ Cons <$> (terminal '\'' "quote" <|> terminal '`' "quasiquote")
  where terminal c s = ([Term $ LIdent s] ++) <$> quoted c
        quoted c = pSym c *> liftL (pSexp <|> pTerm <|> pQuote)

pSpaces :: Parser String
pSpaces = const "" <$> pMany (pAnySym " \t\r\n")

pComment :: Parser Terminal
pComment = LComment <$>
    ((const "" <$> pToken ";|" <* pSexp)
    <<|> (pToken ";#" *> pMunch (/= '\n'))
    <<|> (const "" <$> (pToken ";" *> pMunch (/= '\n'))))
    <?> "Comment"

lexeme :: ParserTrafo a a
lexeme p = p <* pSpaces

parseLisp :: String -> String -> Sexp
parseLisp inputName = runParser inputName p
  where p = pSpaces *> (Cons <$> pList1 pSexp)
