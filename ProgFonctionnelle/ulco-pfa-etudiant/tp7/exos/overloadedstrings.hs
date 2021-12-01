<<<<<<< HEAD

newtype Person = Person String deriving Show
=======
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T 

newtype Person = Person T.Text deriving Show
>>>>>>> try to fix git error

persons :: [Person]
persons = [Person "John", Person "Haskell"]

main :: IO ()
main = print persons

<<<<<<< HEAD
=======

{-
without Overloaded Text :

import qualified Data.Text as T 

newtype Person = Person T.Text deriving Show

persons :: [Person]
persons = [Person (T.pack "John"), Person (T.pack "Haskell")]

main :: IO ()
main = print persons


-}
>>>>>>> try to fix git error
