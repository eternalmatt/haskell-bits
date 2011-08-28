
mergesort :: (Ord a) => [a] -> [a]
mergesort xs = mergeLists (breakup xs)


breakup :: [a] -> [[a]]
breakup []     = []
breakup (x:xs) = [x] : breakup xs


mergeLists :: (Ord a) => [[a]] -> [a]
mergeLists []            = []
mergeLists [xs]          = xs
mergeLists [xs,ys]       = merge xs ys
mergeLists (xs:ys:other) = mergeLists ( merge xs ys : [mergeLists other] )
                         
                         
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
