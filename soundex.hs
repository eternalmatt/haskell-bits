import Data.List
import Data.Maybe

letters = ["BFPV","CGJKQSXZ","DT","L","MN","R"]



guide :: Char -> Int
guide c = fromMaybe 0 $ findIndex (elem c) letters

--code :: String -> String
--code full@(n:_) = [n] ++ take 3 (tail . rules full)



a[]x=x;a(f:s)x=f$a s x


--rules input = delete 0 (removeDups (map (show . guide) input (removeDups (rule5 (rule4 input))))) ++ cycle "0"





rule4 :: (Eq a) => [a] -> [a]
rule4 (a:m@(b:c:xs))
    | guide a == guide c && elem b "AEIOU" =     rule4 m 
    | otherwise                            = a : rule4 m
rule4 xs = xs



rule5 :: (Eq a) => [a] -> [a]
rule5 (a:b:c:xs)
    | guide a == guide c && elem b "HW" = a : rule4 (b:  xs)
    | otherwise                         = a : rule4 (b:c:xs)
rule5 xs = xs



rule1 = removeDups

rule2 (a:b:xs) 
    | guide a == guide b = a : rule2 xs
    | otherwise          = a : rule2 (b:xs)
rule2 xs = xs


removeDups name = foldr (\x y -> if x == head y then y else [x]++y) "" name