module Main where

import Control.Monad.State
import Data.Tuple.Select

-- Lets simulate the synchronisation variables a, b, and c with putStrLn and getLine
-- The dependeny of the threads is determined by their order in the program
main1 :: IO ()
main1 = putStrLn "compiled"
prog1 =  putStrLn "a ?" >>
        getLine           >>=  (\a -> 
        putStrLn ("read " ++ a ++ "\n b ?") >>
        getLine           >>= (\b -> 
        putStrLn ("read " ++ b ++ " a:" ++ a ++ "  c ?") >>
        getLine           >>= (\c -> 
        putStrLn ("read " ++ c ++ " a:" ++ a ++ " b:" ++ b ++ "  new a?") >>
        prog1)))

-- The variable a is out of b's scope so the input sequence has to be  a,  b,  c
-- This is also the dependency
-- To decouple the order of input from the dependencies we use exceptions

-- The Exceptions seem to be unable to get out of their IO. So we use StateT
-- Input- and programming sequences are decoupled
-- The State is mutated for simpler handling
main :: IO ()
main =  let circles = runStateT $ circle 0
        in circles ([Vars 0 0 0],[Vars 0 0 1]) >>
        return ()

-- fetch from previous output
fetch :: Int -> (SType -> SType)
fetch n
    | n <=  1 = \s -> (let var3out = var3 $ head $ sel2 s
                       in case var3out of
                            0 -> (ignoreEmpty' $ sel1 s) <> (ignoreEmpty $ sel1 s)
                            _ -> [Vars (var3 $ head $ sel2 s) 0                            0                     ] <> (ignoreEmpty $ sel1 s)
                        , [Vars (var1 $ last $ sel2 s) (var2 $ last $ sel2 s)  0                   ])
    | n ==  2 = \s -> (let var1out = var1 $ head $ sel2 s
                       in case var1out of
                            0 -> (ignoreEmpty' $ sel1 s) <> (ignoreEmpty $ sel1 s)
                            _ -> [Vars 0                     (var1 $ head $ sel2 s)         0                     ] <> (ignoreEmpty $ sel1 s)
                        , [Vars 0                      (var2 $ last $ sel2 s) (var3 $ last $ sel2 s)])
    | n >=  3 = \s -> (let var2out = var2 $ head $ sel2 s
                       in case var2out of
                            0 -> (ignoreEmpty' $ sel1 s) <> (ignoreEmpty $ sel1 s)
                            _ -> [Vars 0                      0                             (var2 $ head $ sel2 s)] <> (ignoreEmpty $ sel1 s)
                        , [Vars (var1 $ last $ sel2 s) 0                       (var3 $ last $ sel2 s)])

-- input with console
input :: Int -> Int -> (SType -> SType)
input n v
    | n <=  1 = \s -> (case v of
                            0 -> (ignoreEmpty' $ sel1 s) <> (ignoreEmpty $ sel1 s)
                            _ -> [Vars v                       0                       0                    ] <> (ignoreEmpty $ sel1 s)
                        , sel2 s)
    | n ==  2 = \s -> (case v of
                            0 -> (ignoreEmpty' $ sel1 s) <> (ignoreEmpty $ sel1 s)
                            _ -> [Vars 0                       v                       0                    ] <> (ignoreEmpty $ sel1 s)
                        , sel2 s)
    | n >=  3 = \s -> (case v of
                            0 -> (ignoreEmpty' $ sel1 s) <> (ignoreEmpty $ sel1 s)
                            _ -> [Vars 0                       0                       v                    ] <> (ignoreEmpty $ sel1 s)
                        , sel2 s)

-- Compute
prog :: Int -> (Int -> Int) -> (SType -> SType)
prog n f
    | n <=  1   = \s -> (sel1 s, [Vars (f (var1 $ last $ sel1 s))   (var2 $ last $ sel2 s)     (var3 $ last $ sel2 s)]) 
    | n ==  2   = \s -> (sel1 s, [Vars    (var1 $ last $ sel1 s) (f (var2 $ last $ sel2 s))    (var3 $ last $ sel2 s)]) 
    | n >=  3   = \s -> (sel1 s, [Vars    (var1 $ last $ sel1 s)    (var2 $ last $ sel2 s)  (f (var3 $ last $ sel2 s))]) 


program1 :: Int -> Int
-- program1 x = rem (x+1) 3
program1 x = x+1
program2 x = x+1
program3 x = x+1

t1 = run 1 3 (fetch 1) (prog 1 program1)
t2 = run 2 3 (fetch 2) (prog 2 program2)
t3 = run 3 3 (fetch 3) (prog 3 program3)

run ::  Int ->  -- index of thread
        Int ->  -- total quantity of threads
        (SType -> SType) ->
        (SType -> SType) ->
        StateType
run this n fetch prog = -- prog setL hasValue cont uncont
    lift (putStrLn "--") >>
    ps "fetch" >>
    get >>= \varss ->
    put (fetch varss) >> putStateLn >>
    ps "input" >>
    lift getLine    >>= \s ->
    let x = if length s == 0 then 0 else (read [head s])
    in get >>= \varss' ->
    put (input (length s) x varss') >> putStateLn
    
   

circle :: Int -> StateType
circle n        
    | n > 0 &&
      n < 2     -- Chain the read dependencies with >>.
                = t1  >>
                  t2  >>
                  t3  >>
                  circle (n+1)
    | n == 0    = lift (putStrLn "Please hit any key 1 time, 2 times, ... to choose a thread for input.\n     i  i o") >> putStateLn >>
                  circle (n+1)
    | otherwise = lift (putStrLn "End") >> lift (putStrLn "") >> lift (putStrLn "")
-- n: "total CPU cycles" due to termination behaviour of gchi

data VarsType = Vars { var1 :: Int
                       , var2 :: Int
                       , var3 :: Int
                       }
                        deriving (Show)

type SType = ([VarsType],[VarsType])

type StateType = StateT SType IO ()

putStateLn :: StateType
putStateLn =    lift (putStr $ "var1") >> putStateVarLn var1 >>
                lift (putStr $ "var2") >> putStateVarLn var2 >>
                lift (putStr $ "var3") >> putStateVarLn var3

putStateVarLn :: (VarsType -> Int) -> StateType
putStateVarLn f =  get >>= \varss ->
        lift (putStr $ show $ fmap f $ sel1 varss) >>
        lift (putStrLn $ show $ fmap f $ sel2 varss)


ps :: String -> StateType
ps s = lift (putStrLn $ s)

pn :: Int -> StateType
pn n = lift (putStrLn $ show n)

ignoreEmpty :: [VarsType] -> [VarsType]
ignoreEmpty varss = let vars = head varss
                    in if var1 vars == 0 && var2 vars == 0 && var3 vars == 0
                    then []
                    else varss

ignoreEmpty' :: [VarsType] -> [VarsType]
ignoreEmpty' varss =    let vars = head varss
                        in if var1 vars == 0 && var2 vars == 0 && var3 vars == 0
                        then [Vars   0   0   0]
                        else []
