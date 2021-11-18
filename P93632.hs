eql :: [Int] -> [Int] -> Bool
eql xs ys = length ys == length xs && (and $ zipWith (==) xs ys)

prod :: [Int] -> Int
prod = foldr (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens xs = prod $ filter even xs

powersOf2 :: [Int]
powersOf2 = iterate (* 2) 2

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = sum $ zipWith (*) xs ys