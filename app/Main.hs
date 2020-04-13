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
main =  let circles = runStateT $ circle 0
        in circles ([Vars 0 0 0]) >>
        return ()

data VarsType = Vars { var1 :: Int
                       , var2 :: Int
                       , var3 :: Int
                       }
                        deriving Show
       
type StateType = StateT [VarsType] IO ()

circle :: Int -> StateType
circle n
    | n >= 3      = lift $ putStrLn "End"
    | otherwise   = t1 >> putStateLn >>
                    t2 >> putStateLn >>
                    t3 >> putStateLn >>
                    circle (n+1) 

-- \x -> x are the programs generating the output x which is also
-- their new state
t1 = thd "a?" $ thdPure1 (\x -> x)
t2 = thd "b?" $ thdPure2 (\x -> x)
t3 = thd "c?" $ thdPure3 (\x -> x)

thdPure1 :: (Int -> Int) -> (Int -> VarsType -> VarsType)
thdPure1 f = (\a -> \s -> Vars (f a)     (var2 s) (var3 s))

thdPure2 :: (Int -> Int) -> (Int -> VarsType -> VarsType)
thdPure2 f = (\b -> \s -> Vars (var1 s) (f b)    (var3 s)) 

thdPure3 :: (Int -> Int) -> (Int -> VarsType -> VarsType)
thdPure3 f = (\c -> \s -> Vars (var1 s) (var2 s) (f c)   ) 

putStateLn :: StateType
putStateLn = putStateVarLn var1 >> putStateVarLn var2 >> putStateVarLn var3

putStateVarLn :: (VarsType -> Int) -> StateType
putStateVarLn f =  get >>= \sts ->
        lift (putStrLn $ show $ fmap f sts)

thd ::  String ->
        (Int -> VarsType -> VarsType) ->
        StateType
thd msg toSt' =
        lift (putStrLn msg)                 >>
        get                                 >>= \sts ->
        lift getLine                        >>= \a' ->
        let st = last $ sts
        in modify (<> [toSt' (read a') st])

