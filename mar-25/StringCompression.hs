module StringCompression where

-- https://www.hackerrank.com/challenges/string-compression/problem

compress :: String -> String
compress [] = []
compress (x:xs) = compressHelper x 1 xs

compressHelper :: Char -> Int -> String -> String
compressHelper ch count [] = if count == 1
                                      then [ch]
                                      else ch : show count
compressHelper ch count (y:ys)
    | y == ch = compressHelper ch (count + 1) ys
    | otherwise = if count == 1
                  then ch : compressHelper y 1 ys
                  else ch : show count ++ compressHelper y 1 ys
