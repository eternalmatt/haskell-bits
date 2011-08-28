module Parallel 
( parsum
, parmap
) where

import Control.Parallel (par, pseq)



numbers = [1..100] ++ [1,4..3000] ++ [99,92..(-4000)]

parsum :: (Num a) => [a] -> a
parsum []  = 0
parsum xs  | cutoff    = sum xs
           | otherwise = p `par` (q `pseq` (p + q))
           where p = parsum front
                 q = parsum back
                 (front, back) = splitAt' half xs
                 half = length xs `div` 2
                 cutoff = length xs < (20 :: Int)
                 
parmap :: (a -> b) -> [a] -> [b]
parmap _ [] = []
parmap f xs | cutoff    = map f xs
            | otherwise = p `par` (q `pseq` (p ++ q))
            where p = parmap f front
                  q = parmap f back
                  (front, back) = splitAt' half xs
                  half = length xs `div` 2
                  cutoff = length xs < (20 :: Int)

                  
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []     = ([], [])
splitAt' n (x:xs) | n <= 0    = ([], x:xs)
                  | otherwise = let (front, back) = splitAt' (n-1) xs
                                in (x : front, back)
                 
parLength :: [a] -> Int
parLength []       = 0
parLength (_:_:_:_:xs) = 4 + parLength xs
parLength (_:xs)= 1 + parLength xs
