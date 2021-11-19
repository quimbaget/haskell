flatten :: [[Int]] -> [Int]
flatten xs = foldr (++) [] xs

myLength :: String -> Int
myLength = length

myReverse