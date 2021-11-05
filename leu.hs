menor :: Int -> Int -> Int 
menor a b 
    | a<=b = a
    | True = b

menorL :: [Int] -> Int
menorL x = foldr (menor) 1000000 x