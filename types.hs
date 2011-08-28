module Types
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
, Person
) where

import qualified Data.Map as Map

data Shape = Circle Point Float 
           | Rectangle Point Point
           deriving (Show)
           
data Point = Point Float Float deriving (Show)
origin = Point 0 0
           
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Point -> Shape
nudge (Circle (Point x y) r) (Point a b) = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point a b) = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle = Circle origin

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle origin $ Point width height

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
                     
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
           
           
data LockerState = Taken | Free deriving (Show, Eq)

type Code      = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing            -> Left $ "Locker number" ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state == Taken
                              then Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
                              else Right code

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x:-:xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
    | x == y = Node x left right
    | x <  y = Node y (treeInsert x left) right
    | x >  y = Node y left (treeInsert x right)

    
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y left right)
    | x == y = True
    | x <  y = treeElem x left
    | x >  y = treeElem x right
    
    
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red       = True
    Yellow == Yellow = True
    Green == Green   = True
    _ == _           = False
    
instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"
    

class YesNo a where
    yesno :: a -> Bool
    
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
    
instance YesNo [a] where
    yesno [] = False
    yesno _  = True
    
instance YesNo Bool where
    yesno = id
    
instance YesNo (Maybe a) where
    yesno Nothing  = False
    yesno (Just _) = True
    
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True
    
instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True
    
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf val yes no = if yesno val then yes else no

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
