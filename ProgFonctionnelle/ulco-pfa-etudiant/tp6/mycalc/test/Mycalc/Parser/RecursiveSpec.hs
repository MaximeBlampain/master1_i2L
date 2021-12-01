module Mycalc.Parser.RecursiveSpec (main, spec) where

import Test.Hspec

import Mycalc.Parser.Recursive
import Mycalc.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    ---------------------------------------------------------------------------
    -- pDigit :: String -> Result Char
    ---------------------------------------------------------------------------

    describe "pDigit" $ do
        it "1" $ pDigit "0" `shouldBe` Right ('0', "")
        it "2" $ pDigit "0foobar" `shouldBe` Right ('0', "foobar")
        it "3" $ pDigit "foobar" `shouldBe` Left "not a digit"
        it "4" $ pDigit "" `shouldBe` Left "no input"

    ---------------------------------------------------------------------------
    -- pDigits :: String -> Result String
    ---------------------------------------------------------------------------

    describe "pDigits" $ do
        it "1" $ pDigits "0" `shouldBe` Right ("0", "")
        it "2" $ pDigits "42" `shouldBe` Right ("42", "")
        it "3" $ pDigits "42foo" `shouldBe` Right ("42", "foo")
        it "4" $ pDigits "foo42" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- pNat :: String -> Result Int
    ---------------------------------------------------------------------------

    describe "pNat" $ do
        it "1" $ pNat "0" `shouldBe` Right (0, "")
        it "2" $ pNat "42" `shouldBe` Right (42, "")
        it "3" $ pNat "42foo" `shouldBe` Right (42, "foo")
        it "4" $ pNat "foo42" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- pDecimal :: String -> Result Expr
    ---------------------------------------------------------------------------

    describe "pDecimal" $ do
        it "1" $ pDecimal "0" `shouldBe` Right (ExprVal 0, "")
        it "2" $ pDecimal "42" `shouldBe` Right (ExprVal 42, "")
        it "3" $ pDecimal "42foo" `shouldBe` Right (ExprVal 42, "foo")
        it "4" $ pDecimal "foo42" `shouldBe` Left "not a digit"

    ---------------------------------------------------------------------------
    -- pMultitive :: String -> Result Expr
    ---------------------------------------------------------------------------

    describe "pMultitive" $ do
        it "1" $ pMultitive "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22), "")
        it "2" $ pMultitive "20*22*2" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprMul (ExprVal 22) (ExprVal 2)), "")
        it "3" $ pMultitive "20*foo" `shouldBe` Right (ExprVal 20, "*foo")
        it "4" $ pMultitive "foo*20" `shouldBe` Left "not a digit"

{-

    ---------------------------------------------------------------------------
    -- pAdditive :: String -> Result Expr
    ---------------------------------------------------------------------------

    describe "pAdditive" $ do
        it "1" $ pAdditive "20+22" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprVal 22), "")
        it "2" $ pAdditive "20*22" `shouldBe`
                    Right (ExprMul (ExprVal 20) (ExprVal 22), "")
        it "3" $ pAdditive "20+22+2" `shouldBe`
                    Right (ExprAdd (ExprVal 20) (ExprAdd (ExprVal 22) (ExprVal 2)), "")
        it "2" $ pAdditive "20*10+12" `shouldBe`
                    Right (ExprAdd (ExprMul (ExprVal 20) (ExprVal 10)) (ExprVal 12), "")
        it "3" $ pAdditive "20+10*12" `shouldBe`
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

