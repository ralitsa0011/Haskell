--task2 
sumProd :: [[Integer]] -> Integer
sumProd ll = foldr1 (+) [ y | y <- [ foldr1 (*) x | x <- ll ], y > 0 ]

sumProd1 :: [[Integer]] -> Integer
sumProd1 ll = sum [ y | y <- [ product x | x <- ll ], y > 0]

sumProd2 :: [[Integer]] -> Integer
sumProd2 ll = sum [ product xs | xs <- ll, and [x > 0 | x <- xs ] ]

--task2
type Trip = (String, Integer, Float)
type Tour = [Trip]

discount :: Tour -> Integer -> Tour
discount tour len = [ if k > len then (d, k, p / 10) else (d, k, p) | (d, k, p) <- tour ]

Tour1 :: Tour -> String -> String -> Trip -> Tour
Tour1 ((d, k, p) : rest) from to trip
    | d == from = Tour2 rest to trip
    | otherwise = (d, k, p) : Tour1 rest from to trip

Tour2 :: Tour -> String -> Trip -> Tour
Tour2 ((d, k, p) : rest) to trip
    | d == to   = trip : rest
    | otherwise = Tour2 rest to trip
	
--task3
merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = if x < y
                        then x : merge xs (y:ys)
                        else y : merge (x:xs) ys

(\list -> [y | y <- list, even y]) [1, 2, 3, 4]

--task4
sumDigits :: Integer -> Integer
sumDigits n
    | n == 0    = 0
    | n > 0     = n `mod` 10 + sumDigits (n `quot` 10)
    | otherwise = -1