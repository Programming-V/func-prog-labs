module MissingNumbers where

-- https://www.hackerrank.com/challenges/missing-numbers-fp/problem

listA, listB :: [Int]
listA = [203, 204, 205, 206, 207, 208, 203, 204, 205, 206]
listB = [203, 204, 205, 206, 207, 208, 203, 204, 205, 206, 205, 206, 204]

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
    where
        ys = [y | y <- xs, y <= x]
        zs = [z | z <- xs, z > x]

removeDuplicates :: Eq a => a -> [a] -> [a]
removeDuplicates _ [] = []
removeDuplicates x (y:ys)
    | x == y = ys
    | otherwise = y : removeDuplicates x ys

findMissingNumbers :: [Int] -> [Int] -> [Int]
findMissingNumbers [] ys = qsort ys
findMissingNumbers (x:xs) ys = findMissingNumbers xs (removeDuplicates x ys)
