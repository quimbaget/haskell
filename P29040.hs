insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert (x : xs) y
  | y < x = y : x : xs
  | otherwise = x : insert xs y

isort :: [Int] -> [Int]
isort [] = []
isort (x : xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x : xs) y
  | x == y = xs
  | otherwise = x : remove xs y

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = minimum l : ssort (remove l (minimum l))

merge :: [Int] -> [Int] -> [Int]
merge l1 l2 = ssort (l1 ++ l2)

msort :: [Int] -> [Int]
msort [] = []
msort (x : xs) = merge [x] (msort xs)

qsort :: [Int] -> [Int]
qsort xs = qsort' xs []
  where
    qsort' :: [Int] -> [Int] -> [Int]
    qsort' [] result = result
    qsort' [x] result = x : result
    qsort' (x : xs) result = qpart xs [] [] result
      where
        qpart :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
        qpart [] half1 half2 result = qsort' half1 (x : qsort' half2 result)
        qpart (x' : xs') half1 half2 result
          | x' <= x = qpart xs' (x' : half1) half2 result
          | x' > x = qpart xs' half1 (x' : half2) result

genQsort :: Ord a => [a] -> [a]
genQsort xs = genQsort' xs []
  where
    genQsort' [] result = result
    genQsort' [x] result = x : result
    genQsort' (x : xs) result = genQpart xs [] [] result
      where
        genQpart [] half1 half2 result = genQsort' half1 (x : genQsort' half2 result)
        genQpart (x' : xs') half1 half2 result
          | x' <= x = genQpart xs' (x' : half1) half2 result
          | x' > x = genQpart xs' half1 (x' : half2) result