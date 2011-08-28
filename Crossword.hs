words :: [String]
words = ["MACE", "AGE", "CEE", "ESS", "AGES", "WEES"]

puzzle :: [[String]]
puzzle = [head words

elem_at [] _         = error "Index too large"
elem_at xs n | n < 0 = error "negative index"
             | otherwise = elem_at' xs n
             where elem_at' (h:_) 0 = h
                   elem_at' (_:t) n = elem_at t (n-1)
