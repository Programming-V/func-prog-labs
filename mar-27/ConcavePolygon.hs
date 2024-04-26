module ConcavePolygon where

-- https://www.hackerrank.com/challenges/lambda-march-concave-polygon/problem

type Point = (Int, Int)

points :: [Point]
points = [(0, 0), (0, 1), (1, 1), (1, 0)]

getDotProduct :: Point -> Point -> Double
getDotProduct (x1, y1) (x2, y2) = fromIntegral (x1 * x2 + y1 * y2)

getMagnitude :: Point -> Double
getMagnitude (x, y) = sqrt (fromIntegral (x * x + y * y))

getAngle :: Point -> Point -> Point -> Double
getAngle c1 c2 c3 =
                    let
                        dx1 = fst c2 - fst c1
                        dy1 = snd c2 - snd c1
                        dx2 = fst c3 - fst c2
                        dy2 = snd c3 - snd c2

                        dotProd = getDotProduct (dx1, dy1) (dx2, dy2)
                        mag1 = getMagnitude (dx1, dy1)
                        mag2 = getMagnitude (dx2, dy2)

                        cosTheta = dotProd / (mag1 * mag2)
                    in
                        acos (min 1.0 (max (-1.0) cosTheta))

isConcave :: [Point] -> Bool
isConcave [] = False
isConcave [_] = False
isConcave [_, _] = False
isConcave (c1:c2:c3:cs) =
                          let
                              a = getAngle c1 c2 c3
                          in
                              if a > pi
                                  then True
                                  else isConcave (c2:c3:cs)

isConcave' :: [Point] -> String
isConcave' points = if isConcave points
                        then "Yes"
                        else "No"
