module SuperDigit where

-- https://www.hackerrank.com/challenges/super-digit/problem

import Data.Char (digitToInt)

superDigit :: String -> Int -> Int
superDigit d r = superDigitHelper $ concat $ replicate r d

superDigitHelper :: String -> Int
superDigitHelper n | length n == 1 = digitToInt $ head n
                   | otherwise = superDigitHelper $ show $ sumDigits n
    where
        sumDigits :: String -> Int
        sumDigits "" = 0
        sumDigits num@(x:xs) = digitToInt x + superDigitHelper xs
