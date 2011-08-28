





u n=not(elem EQ(once n))&&and(twice n)
once=q compare.show
twice=q(/=).once
q f x=zipWith f x$tail x


{-
undulant :: Int -> Bool
undulant n | length s == 1 = False
           | and . with [GT,LT] || and . with [LT,GT]
    where zipped = zipWith compare s (tail s)
          with   = zipWith (==)    z
          s      = show n
-}