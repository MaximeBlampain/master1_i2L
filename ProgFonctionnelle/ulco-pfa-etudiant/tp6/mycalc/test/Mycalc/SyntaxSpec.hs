module Mycalc.SyntaxSpec (main, spec) where

import Test.Hspec

import Mycalc.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "Expr" $ do
        it "1" $ (ExprVal 0) `shouldBe` (ExprVal 0)
        it "2" $ (ExprAdd (ExprVal 1) (ExprVal 2)) `shouldBe` (ExprAdd (ExprVal 1) (ExprVal 2))
        it "3" $ (ExprMul (ExprVal 1) (ExprVal 2)) `shouldBe` (ExprMul (ExprVal 1) (ExprVal 2))
        it "4" $ (ExprAdd (ExprVal 1) (ExprMul (ExprVal 1) (ExprVal 2)))
                    `shouldBe` (ExprAdd (ExprVal 1) (ExprMul (ExprVal 1) (ExprVal 2)))

        