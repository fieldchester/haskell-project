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
-- Input- and programming sequences are decoupled
main :: IO ()
main =  let circles = runStateT $ circle 0
        in circles ([Vars 1 1 1 0]) >>
        return ()

program1 :: Int -> Int
program1 x = rem (x+1) 3
program2 x = x+1
program3 x = x+1

t1 = run (prog1 program1) (contPred1 (\x ->  x <= 0)) $ cont1
t2 = run (prog2 program2) (contPred2 (\x ->  x <= 0)) $ cont2
t3 = run (prog3 program3) (contPred3 (\x ->  x <= 0)) $ cont3

reset :: Int -> (VarsType -> VarsType)
reset n
    | n <= 1 = \s -> Vars 0         (var2 s) (var3 s) 1
    | n == 2 = \s -> Vars (var1 s)  0        (var3 s) 1 
    | n >= 3 = \s -> Vars (var1 s)  (var2 s)  0       1

circle :: Int -> StateType
circle n        
    | n > 0 &&
      n < 6  
                = t1 >>     putStateLn >> -- Chain the read dependencies.
                  t2 >>     putStateLn >>
                  t3 >>     putStateLn >>
                  circle (n+1)
    | n == 0    = lift (putStrLn "Please hit any key 1 time, 2 times, ... to choose a thread for this input.") >>
                  circle (n+1)
    | otherwise = lift $ putStrLn "End"
-- n: "total CPU cycles" due to termination behaviour of gchi

data VarsType = Vars { var1 :: Int
                       , var2 :: Int
                       , var3 :: Int
                       , t :: Int -- type result 0, input 1, continuation 9
                       }
                        deriving Show

putStateLn :: StateType
putStateLn =    putStateVarLn var1 >> putStateVarLn var2 >> putStateVarLn var3 >>
                putStateVarLn t
       
type StateType = StateT [VarsType] IO ()

prog1 :: (Int -> Int) -> (VarsType -> VarsType)
prog1 f     = \s -> Vars (f $ var1 s) (var2 s)     (var3 s)     0
prog2 f     = \s -> Vars (var1 s)     (f $ var2 s) (var3 s)     0
prog3 f     = \s -> Vars (var1 s)     (var2 s)     (f $ var3 s) 0 


contPred1 :: (Int -> Bool) -> (VarsType -> Bool)
contPred1 p = \s ->    p (var1 s)                                 
contPred2 p = \s ->                  p (var2 s)                   
contPred3 p = \s ->                              p (var3 s)       


cont1 :: (VarsType -> VarsType)
cont1       = \s -> Vars (var1 s)     (var2 s)     (var3 s)      9
cont2       = \s -> Vars (var1 s)     (var2 s)     (var3 s)      9 
cont3       = \s -> Vars (var1 s)     (var2 s)     (var3 s)      9


putStateVarLn :: (VarsType -> Int) -> StateType
putStateVarLn f =  get >>= \sts ->
        lift (putStrLn $ show $ fmap f sts)


run ::  (VarsType -> VarsType) ->
        (VarsType -> Bool) ->
        (VarsType -> VarsType) ->
        StateType
run prog contP cont =
    lift (putStrLn "-----") >>
    get                     >>= \sts ->
    let curr = last sts
        is = contP curr     -- is this a continuation?
    in  let curr' = curr
        in case is of
            False   ->  modify (<> [prog curr'])
            _       ->  modify (<> [cont curr'])        >> -- cont
                        lift (putStrLn "tap x times")   >>
                        lift getLine                    >>= \x ->
                        let lgth = length x
                        in lift (putStrLn $ show lgth)  >> -- input
                        modify (<> [reset lgth curr'])
    -- The idea now is to proceed according to what COULD happen.
