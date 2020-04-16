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
-- The State is mutated for simpler handling
main :: IO ()
main =  let circles = runStateT $ circle 0
        in circles ([Vars 1 1 1 0 0]) >>
        return ()

program1 :: Int -> Int
program1 x = rem (x+1) 3
-- program1 x = x+1
program2 x = x+1
program3 x = x+1

t1 = run 1 3 (prog1 program1) (isValueEmpty1 (\x ->  x <= 0)) $ cont1
t2 = run 2 3 (prog2 program2) (isValueEmpty2 (\x ->  x <= 0)) $ cont2
t3 = run 3 3 (prog3 program3) (isValueEmpty3 (\x ->  x <= 0)) $ cont3

input :: Int -> (VarsType -> [VarsType])
input n
    | n <= 1 = \s -> [Vars 1         (var2 s) (var3 s) 0 1]
    | n == 2 = \s -> [Vars (var1 s)  1        (var3 s) 0 2]
    | n >= 3 = \s -> [Vars (var1 s)  (var2 s)  1       0 3]

circle :: Int -> StateType
circle n        
    | n > 0 &&
      n < 5     -- Chain the read dependencies with >>.
                = t1  >>    putStateLn  >>
                  t2  >>    putStateLn  >>
                  t3  >>    putStateLn  >>
                  circle (n+1)
    | n == 0    = lift (putStrLn "Please hit any key 1 time, 2 times, ... to choose a thread for this input.") >>
                  circle (n+1)
    | otherwise = lift $ putStrLn "End"
-- n: "total CPU cycles" due to termination behaviour of gchi

data VarsType = Vars { var1 :: Int
                       , var2 :: Int
                       , var3 :: Int
                       , iC :: Int -- continuation, if iC is 1, then var1 was empty, 0: no continuation
                       , iI :: Int -- input, if iI is 1, then var1 gets some input, 0: no input
                       }
                        deriving Show

putStateLn :: StateType
putStateLn =    lift (putStr $ "var1") >> putStateVarLn var1 >>
                lift (putStr $ "var2") >> putStateVarLn var2 >>
                lift (putStr $ "var3") >> putStateVarLn var3 >>
                lift (putStr $ "cont") >> putStateVarLn iC >>
                lift (putStr $ "inpt") >> putStateVarLn iI
       
type StateType = StateT [VarsType] IO ()

prog1 :: (Int -> Int) -> (VarsType -> [VarsType])
prog1 f     = \s -> [Vars (f $ var1 s) (var2 s)     (var3 s)     0 1]
prog2 f     = \s -> [Vars (var1 s)     (f $ var2 s) (var3 s)     0 2]
prog3 f     = \s -> [Vars (var1 s)     (var2 s)     (f $ var3 s) 0 3]


isValueEmpty1 :: (Int -> Bool) -> (VarsType -> Bool)
isValueEmpty1 p = \s ->    p (var1 s)                                 
isValueEmpty2 p = \s ->                  p (var2 s)                   
isValueEmpty3 p = \s ->                              p (var3 s)       


cont1 :: (VarsType -> [VarsType])
cont1       = \s -> [Vars (var1 s)     (var2 s)     (var3 s)      1 0]
cont2       = \s -> [Vars (var1 s)     (var2 s)     (var3 s)      2 0]
cont3       = \s -> [Vars (var1 s)     (var2 s)     (var3 s)      3 0]


putStateVarLn :: (VarsType -> Int) -> StateType
putStateVarLn f =  get >>= \sts ->
        lift (putStrLn $ show $ fmap f sts)

-- [Varstype]: [Input Continuation] or -- [Input Continuation Input] or [Input Continuation Input Input] ...
run ::  Int ->  -- index of thread
        Int ->  -- total quantity of threads
        (VarsType -> [VarsType]) ->
        (VarsType -> Bool) ->
        (VarsType -> [VarsType]) ->
        StateType
run this n prog isValueEmpty cont =
    lift (putStrLn "-------") >>
    get                     >>= \sts ->
    case pending sts of
        False ->    case isValueEmpty $ head sts of
                        False ->    put (prog $ head sts) >> lift (putStrLn "-i") -- do a computation
                        _ ->        modify (<> (cont $ head sts)) >> lift (putStrLn "ic") -- place a continuation
        _ ->        lift (putStrLn $ "--thread "++ show this) >> -- each of the following threads can write
                    lift getLine    >>= \s ->    
                    case delta (this - length s) n of
                        0 ->    put (prog $ head sts) >> lift (putStrLn "-resolve pending") -- resolve pending
                        n ->    pend n

pend :: Int -> StateType
pend n = pendHelp 1 n where
  pendHelp i n
    | i <= n =      lift (putStrLn $ "pend"++ show i++" of "++ show n) >>
                    pendHelp (i+1) n
    | otherwise =   lift $ putStr $ ""