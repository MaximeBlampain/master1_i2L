module Mycalc.Parser.MonadicSpec (main, spec) where

import Test.Hspec

import Mycalc.Parser.Monadic
-- import Mycalc.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    ---------------------------------------------------------------------------
    -- itemP :: Parser Char 
    ---------------------------------------------------------------------------

    describe "itemP" $ do
        it "1" $ runParser itemP "0" `shouldBe` Right ('0', "")
        it "2" $ runParser itemP "foobar" `shouldBe` Right ('f', "oobar")
        it "3" $ runParser itemP "" `shouldBe` Left "no input"

    ---------------------------------------------------------------------------
    -- charP :: Parser Char 
    ---------------------------------------------------------------------------

    describe "charP" $ do
        it "1" $ runParser (charP '0') "0" `shouldBe` Right ('0', "")
        it "2" $ runParser (charP 'f') "foobar" `shouldBe` Right ('f', "oobar")
        it "3" $ runParser (charP 'f') "bar" `shouldBe` Left "unexpected 'b', waiting 'f'"
        it "3" $ runParser (charP 'f') "" `shouldBe` Left "no input"

{-

    ---------------------------------------------------------------------------
    -- digitP :: Parser Char
    ---------------------------------------------------------------------------

    describe "digitP" $ do
        it "1" $ runParser digitP "0" `shouldBe` Right ('0', "")
        it "2" $ runParser digitP "0foobar" `shouldBe` Right ('0', "foobar")
        it "3" $ runParser digitP "foobar" `shouldBe` Left "not a digit"
        it "4" $ runParser digitP "" `shouldBe` Left "no input"

    ---------------------------------------------------------------------------
    -- digitsP :: Parser String
    ---------------------------------------------------------------------------

    describe "digitsP" $ do
        it "1" $ runParser digitsP "0" `shouldBe` Right ("0", "")
        it "2" $ runParser digitsP "42" `shouldBe` Right ("42", "")
        it "3" $ runParser digitsP "42foo" `shouldBe` Right ("42", "foo")
        it "4" $ runParser digitsP "foo42" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- natP :: Parser Int
    ---------------------------------------------------------------------------

    describe "natP" $ do
        it "1" $ runParser natP "0" `shouldBe` Right (0, "")
        it "2" $ runParser natP "42" `shouldBe` Right (42, "")
        it "3" $ runParser natP "42foo" `shouldBe` Right (42, "foo")
        it "4" $ runParser natP "foo42" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- decimalP :: Parser Expr
    ---------------------------------------------------------------------------

    describe "decimalP" $ do
        it "1" $ runParser decimalP "0" `shouldBe` Right (ExprVal 0, "")
        it "2" $ runParser decimalP "42" `shouldBe` Right (ExprVal 42, "")
        it "3" $ runParser decimalP "42foo" `shouldBe` Right (ExprVal 42, "foo")
        it "4" $ runParser decimalP "foo42" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- multitiveP :: Parser Expr
    ---------------------------------------------------------------------------

    describe "multitiveP" $ do
        it "1" $ runParser multitiveP "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22), "")
        it "2" $ runParser multitiveP "20*22*2" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprMul (ExprVal 22) (ExprVal 2)), "")
        it "3" $ runParser multitiveP "20*foo" `shouldBe` Right (ExprVal 20, "*foo")
        it "4" $ runParser multitiveP "foo*20" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- additiveP :: Parser Expr
    ---------------------------------------------------------------------------

    describe "additiveP" $ do
        it "1" $ runParser additiveP "20+22" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprVal 22), "")
        it "2" $ runParser additiveP "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22), "")
        it "3" $ runParser additiveP "20+22+2" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprAdd (ExprVal 22) (ExprVal 2)), "")
        it "2" $ runParser additiveP "20*10+12" `shouldBe`
                    Right (ExprAdd (ExprMul (ExprVal 20) (ExprVal 10)) (ExprVal 12), "")
        it "3" $ runParser additiveP "20+10*12" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprMul (ExprVal 10) (ExprVal 12)), "")

    ---------------------------------------------------------------------------
    -- parseExpr :: String -> Either String Expr
    ---------------------------------------------------------------------------

    describe "ExprVal" $ do
        it "1" $ parseExpr "0" `shouldBe` Right (ExprVal 0)
        it "2" $ parseExpr "42" `shouldBe` Right (ExprVal 42)

    describe "ExprAdd" $ do
        it "1" $ parseExpr "20+22" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprVal 22))
        it "2" $ parseExpr "20+10+12" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprAdd (ExprVal 10) (ExprVal 12)))

    describe "ExprMul" $ do
        it "1" $ parseExpr "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22))
        it "2" $ parseExpr "20*10+12" `shouldBe`
                    Right (ExprAdd (ExprMul (ExprVal 20) (ExprVal 10)) (ExprVal 12))
        it "3" $ parseExpr "20+10*12" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprMul (ExprVal 10) (ExprVal 12)))

-}
