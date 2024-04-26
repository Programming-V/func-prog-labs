module PascalTriangle where

-- https://www.hackerrank.com/challenges/pascals-triangle/problem

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = take n $ map pascalRow [0..]
    where
        pascalRow 0 = [1]
        pascalRow rowNum = zipWith (+) (0 : pascalRow (rowNum - 1)) (pascalRow (rowNum - 1) ++ [0])

{-
    This function prints in this way:

    ghci> printPascalTriangle $ pascalTriangle 4
        "1\n1 1\n1 2 1\n1 3 3 1\n"
-}
printPascalTriangle :: [[Int]] -> String
printPascalTriangle t = unlines $ map (unwords . map show) t

{-
    But this other one which uses IO to print line by line
    due to the use of mapM_ and putStrLn

    ghci> printPascalTriangle' 4
        1
        1 1
        1 2 1
        1 3 3 1
-}
printPascalTriangle' :: Int -> IO ()
printPascalTriangle' n = mapM_ (putStrLn . unwords . map show) (pascalTriangle n)
