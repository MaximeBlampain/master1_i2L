module Mycalc.Parser.MegaparsecSpec (main, spec) where

import Data.Either
import Text.Megaparsec
import Test.Hspec

import Mycalc.Parser.Megaparsec
-- import Mycalc.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    ---------------------------------------------------------------------------
    -- natP :: Parser Int
    ---------------------------------------------------------------------------

    describe "natP" $ do
        it "1" $ parse natP "" "0" `shouldBe` Right 0
        it "2" $ parse natP "" "42" `shouldBe` Right 42
        it "3" $ parse natP "" "42foo" `shouldBe` Right 42
        it "4" $ isLeft $ parse natP "" "foo42"

{-

    ---------------------------------------------------------------------------
    -- decimalP :: Parser Expr
    ---------------------------------------------------------------------------

    describe "decimalP" $ do
        it "1" $ parse decimalP "" "0" `shouldBe` Right (ExprVal 0)
        it "2" $ parse decimalP "" "42" `shouldBe` Right (ExprVal 42)
        it "3" $ parse decimalP "" "42foo" `shouldBe` Right (ExprVal 42)
        it "4" $ isLeft $ parse decimalP "" "foo42" 

    ---------------------------------------------------------------------------
    -- multitiveP :: Parser Expr
    ---------------------------------------------------------------------------

    describe "multitiveP" $ do
        it "1" $ parse multitiveP "" "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22))
        it "2" $ parse multitiveP "" "20*22*2" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprMul (ExprVal 22) (ExprVal 2)))
        it "3" $ parse multitiveP "" "20*foo" `shouldBe` Right (ExprVal 20)
        it "4" $ isLeft $ parse multitiveP "" "foo*20"

    ---------------------------------------------------------------------------
    -- additiveP :: Parser Expr
    ---------------------------------------------------------------------------

    describe "additiveP" $ do
        it "1" $ parse additiveP "" "20+22" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprVal 22))
        it "2" $ parse additiveP "" "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22))
        it "3" $ parse additiveP "" "20+22+2" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprAdd (ExprVal 22) (ExprVal 2)))
        it "2" $ parse additiveP "" "20*10+12" `shouldBe`
                    Right (ExprAdd (ExprMul (ExprVal 20) (ExprVal 10)) (ExprVal 12))
        it "3" $ parse additiveP "" "20+10*12" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprMul (ExprVal 10) (ExprVal 12)))

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

