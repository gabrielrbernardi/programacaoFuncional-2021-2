subStringDeAte :: String -> Int -> Int -> String
subStringDeAte s i t = take t (drop (i) s)