module Main where

import System.Environment (getArgs)
import Data.List.Split (splitOneOf)
import Data.Bits (complement, (.&.), (.|.))
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No IP provided"
        [x] -> calc x
        _ -> putStrLn "Too many arguments"

calc :: String -> IO ()
calc x = do
    let (ip, m) = splitIP x

    printf "First address: %s\n" (printIP (firstAddress ip $ mask m) m)
    printf "Last address: %s\n" (printIP (lastAddress ip $ mask m) m)
    printf "Number of addresses: %d\n" (2^(32 - m) :: Integer)

str2int :: String -> Int
str2int s = read s :: Int

splitIP :: String -> ([Int], Int)
splitIP s = (take 4 numbers, m)
    where
        numbers = map str2int $ splitOneOf "./" s
        m = last numbers

mask :: Int -> [Int]
mask x = replicate (x `div` 8) 255 ++ nonzero ++ replicate ((32 - x - 1) `div` 8) 0
    where
        nonzero = case x `mod` 8 of
            7 -> [254]
            6 -> [252]
            5 -> [248]
            4 -> [240]
            3 -> [224]
            2 -> [192]
            1 -> [128]
            _ -> [0]

firstAddress :: [Int] -> [Int] -> [Int]
firstAddress = zipWith (.&.)

lastAddress :: [Int] -> [Int] -> [Int]
lastAddress ip m = map (+256) $ zipWith (.|.) ip $ map complement m

printIP :: [Int] -> Int -> String
printIP [] _ = ""
printIP [x] m = show x ++ '/' : show m
printIP (x:y) m = show x ++ '.' : printIP y m
