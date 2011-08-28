

pi' = length (takeWhile (/=pi) series)
    where series = [ (2*k)^2 / ((2*k)^2 - 1) | k<-[1..]]
