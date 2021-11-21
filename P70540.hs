data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val a) = a
eval1 (Add a y) = eval1 a + eval1 y
eval1 (Sub a y) = eval1 a - eval1 y
eval1 (Mul a y) = eval1 a * eval1 y
eval1 (Div a y) = eval1 a `div` eval1 y

computeMonad :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Int
computeMonad f x y = do
  i <- eval2 x
  j <- eval2 y
  Just (f i j)

computeDiv :: Expr -> Expr -> Maybe Int
computeDiv _ 0 = Nothing
computeDiv x y = Just (x `div` y)

eval2 :: Expr -> Maybe Int
eval2 (Val a) = return a
eval2 (Add a b) = computeMonad (+) a b
eval2 (Sub a b) = computeMonad (-) a b
eval2 (Mul a b) = computeMonad (*) a b
eval2 (Div a b) = computeDiv a b
