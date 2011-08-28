{-1-}
myLast :: [a] -> a
myLast []  = error "Cannot call last on empty list"
myLast [x] = [x]
myLast (_:xs) = myLast xs

{-2-}
myButLast :: [a] -> a
myButLast []     = error "Cannot call on empty"
myButLast (x:[]) = error "Cannot call on single"
myButLast [x,_]  = x
myButLast (_:x)  = myBustLast xs

{-3-}
elementAt :: (Integral a) => [b] -> a -> b
elementAt [] _     = error "Empty list"
elementAt (x:_)  1 = x
elementAt (_:xs) n
	| n < 1     = error "Lists start at 1"
	| otherwise = elementAt xs (n-1)

{-4-}
myLength :: (Integral a) => [b] -> a
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

{-5-}
reverse :: [a] -> [a]
reverse list = let empty = []
	       in rev list empty
	       where 
		rev [] reversed     = reversed
		rev (x:xs) reversed = rev xs (x:reversed)

{-6-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = equal xs (reverse xs)
	where
	equal [] [] = True
	equal (x:xs) (y:ys)
		| x == y = equal xs ys
		| otherwise = False
		
{-7-}
flatten :: NestedList a -> [a]
flatten (Elem a ) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flattten x ++ flatten (List xs)

{-8-}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)	
	| isInside x xs = x : compress xs
	| otherwise     =     compress xs

isInside :: (Eq a) => a -> [a] -> Bool
isInside _ []     = False
isInside n (x:xs) = if n == x then True
		    else isInside x xs
	
{-9-}
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

{-10-}
encode :: (Eq a, Integral b) => [a] -> [(b, a)]
encode xs = [ (length x, head x) | x <- (pack xs) ]

{-11-}
data ListItem a = Single a | Multiple Int a
	deriving (Show)
	
encodeModified (Eq a, Integral b) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
	where 
		encodeHelper (1,x) = Single x
		encodeHelper (n,x) = Multiple n x

euler1 = sum [ x | x<-[1..1000], mod x 3 == 0] + sum [ x | x <- [1..1000], mod x 5 == 0]

fibonacci :: (Integral a) => a -> a
fibonacci 1 = 1
fibonacci 2 = 2
fibonacci n = fib (n-1) + fib (n-2)

fibSequence = [fibonacci n | n<-[1..]]

euler2 = sum [ x | x <- (take fibNumbers 4000000), mod x 2 == 0]

isPrime :: (Integral a) a -> Bool
isPrime x = if elem True divisible then False else True
	    where divisible = [ if div x n == 0 then True else False | n <- test]
	               test = [2..(sqrt n)]
	               
primes = sieve 2 : 3 : sieve (tail primes) [5,7..]
	where sieve (p:ps) xs = h ++ sieve ps [ x | x <- t, rem x p \= 0]
		       (h, t) = span (< p*p) xs
		       
		       
primeFactors :: (Integral a) -> a -> [a]
primeFactors n = [ x | x <- (take primes (sqrt n)), div x n == 0]

euler3 = maximum (primeFactors 600851475143)

isPalindrome :: (Integral a) => a -> Bool
isPalindrome x = if (show x == x) then True else False

euler4 = maximum [ x * y | x <- [100..999], y <- [100..999], isPalindrome x && isPalindrome y]

divisibleByEach :: (Integral a) => a -> [a] -> Bool
divisibleByEach _ [] = True
divisibleByEach n (x:xs)
	| (div n x == 0) = divisibleByEach n xs
	| otherwise = False

--2520 x y = [ n | n <- [n..], divisibleByEach n [x..y] ]
euler5 = minimum [ n | n <- [1..], divisibleByEach n [1..20] ]

sumOfSquares, squareOfSum :: (Integral a) => [a] -> a
sumOfSquares xs = sum [ x^2 | x <- xs]
squareOfSum  xs = (sum xs)^2

euler6 = if (difference < 0) then (0 - difference) else difference
	 where difference = sumOfSquares ns - squareOfSum ns
	               ns = [1..100]

euler7 = primes !! (10001 - 1)

euler8 = let fiveConsecutive = [ [a,b,c,d,e] | [a,b,c,d,e] <- bigString, a == b, b == c, c == d, d == e]
	 in maximum [ product list | [a,b,c,d,e] <- bigString, let list = readList [a,b,c,d,e] ]
	 where bigNumber = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
	       bigString = show bigNumber
	       
	       
product :: (Integral a) => [a] -> a
product []     = 1
product (x:xs) = x * product xs

readList :: (Integral a) => [String] -> [a]
readList []     = []
readList (x:xs) = read x : readList xs

euler9 = let pyTriplets = [ (a,b,c) | a <- [1..], b <- [1..], c <- [1..], a^2 + b^2 == c^2]
	 in head [ a*b*c | (a,b,c) <- pyTriplets, a + b + c == 1000]
	 
euler10 = sum [ p | p <- (take primes 2000000)]


gridString = ["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08"],["49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00"],["81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65"],["52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91"],["22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80"],["24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50"],["32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70"],["67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21"],["24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72"],["21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95"],["78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92"],["16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57"],["86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58"],["19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40"],["04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66"],["88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69"],["04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36"],["20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16"],["20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54"],["01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]]
row n = [ x | [x,y,' '] <- (grid !! n) ]
grid = [ row x | x <- grid]


