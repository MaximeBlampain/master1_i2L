{-# LANGUAGE FlexibleInstances #-}

module Mycalc.Parser.Monadic where

import Control.Applicative
-- import Data.Char

import Mycalc.Syntax


-------------------------------------------------------------------------------
-- main types & functions
-------------------------------------------------------------------------------

type Result v = Either String (v, String)

newtype Parser v = Parser { runParser :: String -> Result v }

parseExpr :: String -> Either String Expr
parseExpr str = fst <$> runParser additiveP str

itemP :: Parser Char 
itemP = Parser $ \s0 ->
    case s0 of
        (x:xs) -> Right (x, xs)
        [] -> Left "no input"


-------------------------------------------------------------------------------
-- parsing primitives
-------------------------------------------------------------------------------

charP :: Char -> Parser Char
charP s = do
    c <- itemP
    if c == s
    then return c
    else fail ("unexpected " ++ show c ++ ", waiting " ++ show s)

-- TODO digitP :: Parser Char

-- TODO digitsP :: Parser String

-- TODO natP :: Parser Int


-------------------------------------------------------------------------------
-- grammar
-------------------------------------------------------------------------------

-- TODO decimalP :: Parser Expr

-- TODO multitiveP :: Parser Expr

-- TODO
additiveP :: Parser Expr
additiveP = charP '0' >> (return $ ExprVal 0)
    


-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Functor Parser where

    -- (a -> b) -> f a -> f b
    fmap f (Parser p) = 
        Parser $ \d0 -> do
            (x, d1) <- p d0
            return (f x, d1)

instance Applicative Parser where

    -- a -> f a
    pure x = Parser $ \d -> Right (x, d)

    -- f (a -> b) -> f a -> f b
    (Parser p1) <*> (Parser p2) =
        Parser $ \d0 -> do
            (f, d1) <- p1 d0
            (a, d2) <- p2 d1
            return (f a, d2)

instance Monad Parser where

    -- m a -> (a -> m b) -> m b
    (Parser p1) >>= f2 = 
        Parser $ \d0 -> do
            (a, d1) <- p1 d0
            (b, d2) <- runParser (f2 a) d1
            return (b, d2)

    -- fail :: String -> Parser a
    fail msg = Parser (\_ -> Left msg)

instance Alternative (Either String) where

    -- empty :: f a 
    empty = Left "empty"

    -- (<|>) :: f a -> f a -> f a
    Left _ <|> e2 = e2
    e1 <|> _ = e1

instance Alternative Parser where

    -- empty :: f a 
    empty = Parser $ const empty

    -- (<|>) :: f a -> f a -> f a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input


