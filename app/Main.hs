module Main where

-- import Lib
import Prelude hiding (catch)
import Control.Exception


-- Lets simulate the synchronisation variables a, b, and c with putStrLn and getLine
-- The dependeny of the threads is determined by their order in the program
main :: IO ()
main = putStrLn "compiled"
prog =  putStrLn "a ?" >>
        getLine           >>=  (\a -> 
        putStrLn ("read " ++ a ++ "\n b ?") >>
        getLine           >>= (\b -> 
        putStrLn ("read " ++ b ++ " a:" ++ a ++ "  c ?") >>
        getLine           >>= (\c -> 
        putStrLn ("read " ++ c ++ " a:" ++ a ++ " b:" ++ b ++ "  new a?") >>
        prog)))

-- The variable a is out of b's scope so the input sequence has to be  a,  b,  c
-- This is also the dependency
-- To decouple the order of input from the dependencies we use exceptions