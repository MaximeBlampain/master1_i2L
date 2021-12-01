module Mycalc.CodegenSpec (main, spec) where

import Test.Hspec

import Mycalc.Codegen
import Mycalc.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "eval ExprVal" $ do
        it "1" $ eval (ExprVal 0) `shouldBe` 0
        it "2" $ eval (ExprVal 42) `shouldBe` 42

    describe "eval ExprAdd" $ do
        it "1" $ eval (ExprAdd (ExprVal 0) (ExprVal 0)) `shouldBe` 0
        it "2" $ eval (ExprAdd (ExprVal 42) (ExprVal 0)) `shouldBe` 42
        it "3" $ eval (ExprAdd (ExprVal 0) (ExprVal 42)) `shouldBe` 42
        it "4" $ eval (ExprAdd (ExprVal 20) (ExprVal 22)) `shouldBe` 42
        it "5" $ eval (ExprAdd (ExprAdd (ExprVal 9) (ExprVal 11)) (ExprVal 22)) `shouldBe` 42
        
    describe "eval ExprMul" $ do
        it "1" $ eval (ExprMul (ExprVal 0) (ExprVal 42)) `shouldBe` 0
        it "2" $ eval (ExprMul (ExprVal 1) (ExprVal 42)) `shouldBe` 42
        it "3" $ eval (ExprMul (ExprMul (ExprVal 2) (ExprVal 10)) (ExprVal 3)) `shouldBe` 60
        it "4" $ eval (ExprMul (ExprAdd (ExprVal 9) (ExprVal 11)) (ExprVal 3)) `shouldBe` 60
        
    describe "toLisp ExprVal" $ do
        it "1" $ toLisp (ExprVal 0) `shouldBe` "0"
        it "2" $ toLisp (ExprVal 42) `shouldBe` "42"

    describe "toLisp ExprAdd" $ do
        it "1" $ toLisp (ExprAdd (ExprVal 0) (ExprVal 0)) `shouldBe` "(+ 0 0)"
        it "2" $ toLisp (ExprAdd (ExprVal 42) (ExprVal 0)) `shouldBe` "(+ 42 0)"
        it "3" $ toLisp (ExprAdd (ExprVal 0) (ExprVal 42)) `shouldBe` "(+ 0 42)"
        it "4" $ toLisp (ExprAdd (ExprVal 20) (ExprVal 22)) `shouldBe` "(+ 20 22)"
        it "5" $ toLisp (ExprAdd (ExprAdd (ExprVal 9) (ExprVal 11)) (ExprVal 22)) `shouldBe` "(+ (+ 9 11) 22)"
        
    describe "toLisp ExprMul" $ do
        it "1" $ toLisp (ExprMul (ExprVal 0) (ExprVal 42)) `shouldBe` "(* 0 42)"
        it "2" $ toLisp (ExprMul (ExprVal 1) (ExprVal 42)) `shouldBe` "(* 1 42)"
        it "3" $ toLisp (ExprMul (ExprMul (ExprVal 2) (ExprVal 10)) (ExprVal 3)) `shouldBe` "(* (* 2 10) 3)"
        it "4" $ toLisp (ExprMul (ExprAdd (ExprVal 9) (ExprVal 11)) (ExprVal 3)) `shouldBe` "(* (+ 9 11) 3)"
        
