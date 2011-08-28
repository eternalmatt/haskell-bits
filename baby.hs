import Data.List ((\\), any)
import Control.Parallel

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
		      then x
		      else x * 2
		      
conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x]

repeatString         :: Int -> String -> String
repeatString x phrase = take (x * length' phrase + x - 1) (cycle (phrase++" "))

length' xs = sum [1|_<-xs]

removeNonUppercase string = [character | character <- string,
				elem character ['A'..'Z']]
				
evenElements xs = [x | x <- xs, even x]
oddElements xs  = [x | x <- xs, odd  x]

triangles :: [(Int,Int,Int)]
triangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b] ]

rightTriangles :: [(Int,Int,Int)] -> [(Int,Int,Int)]
rightTriangles triangles = [rt | rt <- triangles, isaRightTriangle rt]

isaRightTriangle :: (Int,Int,Int) -> Bool
isaRightTriangle (a,b,c) = if a^2 + b^2 == c^2 then True else False

perimeter :: (Int,Int,Int) -> Int
perimeter (a,b,c) = a+b+c

triWithPerim :: Int -> [(Int,Int,Int)] -> [(Int,Int,Int)]
triWithPerim p t = [x | x <- rightTriangles t, perimeter x == p]


head' :: [a] -> a
head' []    = error "Can't call head on an empty list!"
head' (x:_) = x

capital :: String -> String
capital ""     = "Empty string!"
capital [x]    = "Lolz"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= skinny = "You're underweight"
	| bmi <= normal = "You're normal"
	| bmi <= fat    = "You're fat"
	| otherwise     = "You're a whale"
	where bmi    = weight / height ^ 2
	      skinny = 18.5
	      normal = 25.5
	      fat    = 30
	
max' :: (Ord a) => a -> a -> a
max' a b
	| a > b     = a
	| otherwise = b
	
compare' :: (Ord a) => a -> a -> Ordering
compare' a b
	| a > b      = GT
	| a == b     = EQ
	| otherwise  = LT
	
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
	
	
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
	    topArea  = pi * r ^2
	in  sideArea + 2 * topArea
	
maximum' :: (Ord a) => [a] -> a
maximum' []  = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
      | n <= 0 = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) = if n == x then True else elem n xs

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = 
	let smallerSorted = quicksort [a | a <- xs, a <= x]
	    biggerSorted  = quicksort [a | a <- xs, a >  x]
	in smallerSorted ++ [x] ++ biggerSorted
	
	
alphabet = ['a'..'z']
useableCharacters = [' '..'~']

makeChange :: (Integral a) => a -> [a] -> [a]
makeChange _ []      = []
makeChange 0 s@(_:_) = [ 0 | _ <- s]
makeChange n (x:xs)  = let s = quot n x
		        in s : makeChange (n - s * x) [x]

addPairsPointwise :: (Num a) => [(a,a)] -> (a,a)
addPairsPointwise [] = (0,0)
addPairsPointwise xs = (sum (firsts xs), sum (seconds xs))
				      
firsts, seconds :: (Num a) => [(a,a)] -> [a]
firsts []         = []
firsts ((a,_):xs) = a : firsts xs
seconds []         = []
seconds ((_,b):xs) = b : seconds xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort []     = []
insertionSort (x:xs) =  myInsert x (insertionSort xs)
	where myInsert :: (Ord a) => a -> [a] -> [a]
	      myInsert y [] = [y]
	      myInsert y all@(z:zs)
	      		| y > z     = z : myInsert y zs
	      		| otherwise = y : all
	      	
euler1 = sum [ x | x<-[1..1000], mod x 3 == 0 || mod x 5 == 0]

primes    = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve (p:ps) x k = [n | n <- [x+2,x+4..p*p-2], and [rem n p/=0 | p <- take k oddprimes]] ++ sieve ps (p*p) (k+1)


euler3 = [ x | x<-(takeWhile (<(600851475143)) primes), div 600851475143 x == 0]

initial = head [ (x,y) | x <- [1..], y <- [1..x], banker x y 20 == 1000000 ]
 
banker :: Int -> Int -> Int -> Int
banker x y = helper
             where helper n | n == 1    = x
                            | n == 2    = y
                            | otherwise = let (fibMin3, fibMin2) = fib (n-3)
                                          in  x * fibMin3 + y * fibMin2
 
 
-- Algorithm found at http://www.haskell.org/haskellwiki/The_Fibonacci_sequence#Another_fast_fib
-- Return (fib n, fib (n + 1))
fib :: Int -> (Int, Int)
fib 0 = (1, 1)
fib 1 = (1, 2)
fib n | even n    = let a_sq = a*a in (a_sq + b*b, c*c - a_sq)
      | otherwise = let c_sq = c*c in (c_sq - a*a, b*b + c_sq)
      where (a,b) = fib (n `div` 2 - 1)
            c     = a + b

filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

filterMany :: [a -> Bool] -> [a] -> [a]
filterMany ps xs = foldr filter xs ps

sum' :: (Num a) => [a] -> a
sum' []  = 0
sum' [x] = x
sum' xs  | cutoff    = sum xs
         | otherwise = p `par` (q `pseq` (p + q))
         where p = sum' front
               q = sum' back
               front = take half xs
               back = xs \\ front
               half = length xs `div` 2
               cutoff = length xs > (20 :: Int)
               
while :: (a -> Bool) -> [a] -> Bool
while _ []     = True
while f (x:xs) | f x       = while f xs
               | otherwise = False


numbers = [1..10] ++ [1,3..500] ++ [99,5..2000] :: [Int]


isUpper :: Char -> Bool
isUpper x = x `elem` ['A'..'Z']


--breakup :: a list of orderable elements -> a pair of lists, each containing a pair of lists (recursively, ultimately ending in (a:[],[]))
{-
breakup :: [a] -> [b,b]
breakup [] = (,)
breakup xs = let halfway       = length xs `div` 2
                 (left, right) = splitAt halfway xs
             in (breakup left, breakup right)

merge :: ([b],[b]) -> [a]
merge ([],[])   = []
merge (xs,[])   = xs
merge ([],ys)   = ys
merge ([x],[y]) = if x < y then [x,y] else [y,x]
merge (xs,ys)   = merge xs ++ merge ys


mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs = merge $ breakup xs
-}


mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs = mergeMany $ breakup xs


merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
     | x < y     = x : y : merge xs ys
     | otherwise = y : x : merge xs ys


mergeMany []            = []
mergeMany [xs,ys]       = merge xs ys
--mergeMany (xs:ys:other) = mergeMany ( merge xs ys : mergeMany other)


breakup :: [a] -> [[a]]
breakup []       = [[]]
breakup [x]      = [[x]]
breakup (x:y:xs) = [x,y] : breakup xs


something xs = case xs of []     -> [1]
                          (x:xs) -> x : xs

