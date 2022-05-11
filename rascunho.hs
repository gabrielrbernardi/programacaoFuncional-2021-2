provaSub :: Int -> [(Int, Int)]
provaSub val
    | val == 1 = [(1,1)]
    | (val^2, val) : provaSub(val-1)
