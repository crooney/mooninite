{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts, RankNTypes #-}
module Parser (
      Exp
    , parseLisp
    ) where

import           Text.ParserCombinators.UU.Derived
import           Text.ParserCombinators.UU.Core
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils

data Exp = LAtom String
         | LIdent String
         | LString String
         | LNumber Double
         | LCons Exp Exp
         | LNil
         | LBool Bool
         | LQuote Exp
         | LQuasi Exp
         | LList [Exp]
         | LComment String
  deriving Show

(<++>) :: Parser String -> Parser String -> Parser String
p <++> q = (++) <$> p <*> q

liftS :: ParserTrafo Char String
liftS = ((:[]) <$>)

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
pName = lexeme $ cleanChars <$> (liftS (pLetter <|> pAnySym "*")
        <++> pMany (pDigit <|> pLetter <|> pAnySym (".!?" ++ badChars)))

pIdent :: Parser Exp
pIdent = lexeme $ go <$> pName <?> "Identifier"
  where go "nil" = LNil
        go x     = LIdent x

pAtom :: Parser Exp
pAtom = lexeme $ go <$> (liftS (pSym '#') <++> pName) <?> "Atom"
  where go "#f" = LBool False
        go x    = LAtom x

pNumber :: Parser Exp
pNumber = lexeme $ LNumber <$> pDouble <?> "Number"

pString :: Parser Exp
pString = lexeme $ LString <$> pQuotedString <?> "Normal String"

pMultiLine :: Parser String
pMultiLine = pBrackets (pBrackets (pMunch (/= ']')))

pRawString :: Parser Exp
pRawString = lexeme $ LString <$> pMultiLine
             <?> "Raw String"

pSexp :: Parser [Exp]
pSexp = lexeme $ pParens (pList $ lexeme $
    foldr1 (<|>) [pAtom, pIdent, pNumber, pString, pRawString, pQuote , pQuasi,
                  liftList pSexp, pComment])
    <?> "S-expression"

liftList :: ParserTrafo [Exp] Exp
liftList = (LList <$>)

pQuoted :: (Exp -> Exp) -> Char -> Parser Exp
pQuoted f c = lexeme $ pSym c *> (liftList (walkQuotes f <$> pSexp)
              <|> (f <$> (pIdent <|> pAtom))) <?> c : " Quote"

pQuote :: Parser Exp
pQuote = pQuoted LQuote '\''

pQuasi :: Parser Exp
pQuasi = pQuoted LQuasi '`'

pComment :: Parser Exp
pComment = lexeme $ LComment <$>
    ((const "" <$> pToken ";|" <* pSexp)
    <<|> (pToken ";#" *> pMunch (/= '\n'))
    <<|> (const "" <$> (pToken ";" *> pMunch (/= '\n'))))
    <?> "Comment"

walkQuotes :: (Exp -> Exp) -> [Exp] -> [Exp]
walkQuotes f = map go
  where go (LList x)  = LList (walkQuotes f x)
        go (LIdent x) = f $ LIdent x
        go (LAtom x)  = f $ LAtom x
        go x          = x

parseLisp :: String -> String -> Exp
parseLisp inputName = runParser inputName p
  where p = pSpaces *> liftList (pList1 (liftList pSexp))
