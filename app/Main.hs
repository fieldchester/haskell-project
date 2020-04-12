module Main where

import Control.Monad.State

-- Lets simulate the synchronisation variables a, b, and c with putStrLn and getLine
-- The dependeny of the threads is determined by their order in the program
main1 :: IO ()
main1 = putStrLn "compiled"
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

-- The Exceptions seem to be unable to get out of their IO. So we use StateT
main :: IO ()
main =  let run = runStateT program
        in run ([Vars 0 0 0]) >>
        return ()

data VarsType = Vars { var1 :: Int
                       ,var2 :: Int
                       ,var3 :: Int }

instance Show VarsType where
    show (Vars x y z) = (show x)++"-"++(show y)++"-"++(show z)
       
type StateType = StateT [VarsType] IO ()

program :: StateType
program = circle 0

circle :: Int -> StateType
circle n
    | n >= 3      = lift $ putStrLn "End"
    | otherwise   = t1 >> -- depends on t2's output
                    t2 >> -- dempends on t3's output
                    t3 >> -- dempends on t1's output
                    circle (n+1) 

t1 = thread "a?" (\a -> \s -> Vars a        (var2 s) (var3 s))
t2 = thread "b?" (\b -> \s -> Vars (var1 s) b        (var3 s))
t3 = thread "c?" (\c -> \s -> Vars (var1 s) (var2 s) c       )

thread :: String -> (Int -> VarsType -> VarsType) -> StateType
thread message new =
                    lift (putStrLn message)           >>
                    get                               >>= \sts ->
                    lift getLine                      >>= \a' ->
                    let st = last sts
                    in modify (<> [new (read a') st]) >>      -- update State
                    get                               >>= \sts' ->
                    lift (putStrLn $ show sts')
