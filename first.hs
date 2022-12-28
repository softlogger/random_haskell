--import Data.Functor
--import Data.List (intercalate)
--import Control.Monad.Trans.State


module Main where
main :: IO ()
main = putStrLn "blah"
jazz :: Int -> Int
jazz n = n * 2

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma (xs)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]


fr :: (a -> b -> b) -> b -> [a] -> b
fr f v [] = v
fr f v (x:xs) = f x (fr f v xs)

-- Add (Val 1) (Val 2)
data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving (Show, Eq, Ord)

isAdd :: Expr -> Bool
isAdd (Add _ _) = True
isAdd _ = False

size :: Expr -> Int

foldsz :: (Int -> a) -> (a -> a -> a) -> Expr -> a
foldsz f g (Val _) = (f 1)
foldsz f g (Add x y) = g (foldsz f g x) (foldsz f g y)
foldsz f g (Mult x y) = g (foldsz f g x) (foldsz f g y)

size = foldsz id (+)

foldev :: (Int -> a) -> (a -> a -> a) -> Expr -> a
foldev f g (Val n) = (f n)
foldev f g (Add x y) = g (foldev f g x) (foldev f g y)
foldev f g (Mult x y) = g (foldev f g x) (foldev f g y)

eval :: Expr -> Int
eval xp = foldev id g xp
        where g = if isAdd xp then (+)  else (*)



act :: IO (String, String)
act = do x <- getLine
         putStrLn " *** "
         y <- getLine
         putStrLn " **** "
         return (x, y)

strlen :: IO ()
strlen = do putStrLn "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

-- [1,2,3] -> [1,4,9]
revrev :: [Int] -> [Int]
revrev = map (\x -> x ^ 2)

-- [1,2,3] -> [2,3,4]
therev :: [Int] -> [Int]
therev = map (\x -> x + 1)

allt1 :: [Int] -> [Int]
allt1 = revrev.therev

allt2 :: [Int] -> [Int]
allt2 = therev.revrev

-- safediv :: Integral a => a -> a -> Maybe a
-- safediv a b =
--     if b == 0 then Nothing else Just $ div a b

triple :: (a -> a) -> (a -> a) -> (a -> a) -> a -> a
triple f g h a = f (g ( h (a)))

tripled :: (a -> a) -> (a -> a) -> (a -> a) -> a -> a
tripled f g h a = f $ g $ h a

{-
($)     :: (a -> b) -> a -> b
fmap: (<$>)   :: (a -> b) -> f a -> f b
applicative functor sequential application: (<*>) :: f (a -> b) -> f a -> f b
bind: (>>=) :: (a -> m b) -> m a -> m b
 -- Nothing >>= f >>= Nothing
 -- Just x >>= f >>= f x
-}

-- data Bs = Bs Int | Nothing deriving (Show, Eq, Ord)
-- -- class Functor Bs a  where 
-- --    fmap :: (a -> b) -> Bs a -> Bs b
-- instance Main.Functor Bs where
--     fmap :: func (Bs a) = Bs (func a)
--     fmap :: func Main.Nothing = Main.Nothing
--     (<$) :: a -> Bs b -> Bs a
-- --(<$) :: a -> f b -> f a




data Bs a = Bs a | Nothing deriving (Show, Eq, Ord)
instance Functor Bs where
    fmap f (Bs a) = Bs (f a)
    fmap f Main.Nothing = Main.Nothing
instance Applicative Bs where
    -- pure  = Bs 
    pure a = Bs a
    --(Bs f) <*> (Bs b) = (Bs (f b))
    Main.Nothing <*> _ = Main.Nothing
    (Bs f) <*> (Bs b) = fmap f (Bs b)
    -- (Bs f) <*> (Bs b) <*> (Bs b) = (Bs f) <*> (Bs b)
instance Monad Bs where
    return = pure
    Main.Nothing >>= _  = Main.Nothing
    (Bs a) >>= f = f a

half :: (Integral a) => a -> Bs a
half x = if even x
           then Bs (x `div` 2)
           else Main.Nothing


dbl :: (Num a) => a -> (Bs a)
dbl x = Bs (x * 2)

czy :: (Show a) => a -> Bs String
czy x = Bs (show x)
--dbl Main.Nothing = Main.Nothing

--(>>=) :: m a -> (a -> m b) -> m b


-- > Just 3 >>= half
-- Nothing
-- > Just 4 >>= half
-- Just 2
-- > Nothing >>= half
-- Nothing

-- (Bs 10) >>= (\x -> Bs (x + 100)) >>= (\x ->  Bs (x + 500)) 
-- (Bs 10) >>= (\x -> Bs (x * x)) >>= (\x -> Bs (x * x))
-- (Bs "I ") >>= (\x -> Bs (x ++ " LOVE ")) >>= (\x -> Bs (x ++ " Freaking MONADS"))
getval :: Bs a -> a
getval (Bs a) = a


-- https://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
-- Conclusion
-- A functor is a data type that implements the Functor typeclass.
-- An applicative is a data type that implements the Applicative typeclass.
-- A monad is a data type that implements the Monad typeclass.
-- A Maybe implements all three, so it is a functor, an applicative, and a monad.

-- functors: you apply a function to a wrapped value using fmap or <$> -- fmap (+100) (Bs 10)
-- applicatives: you apply a wrapped function to a wrapped value using <*> or liftA -- ghci> (Bs (+)) <*> (Bs 100) <*> (Bs 1000)
-- monads: you apply a function that returns a wrapped value, to a wrapped value using >>= or liftM
--  (Bs 10) >>= (\x -> Bs (x + 100)) >>= (\x ->  Bs (x + 500)) 



ca :: Int -> Int 
ca n = n * 2

cb :: Int -> Int
cb n = n - 10

snum :: Int -> Int
snum n | n < 1 = - 1
       | n < 10 = 5
       | otherwise = 1000

{-
class Functior f where
    fmap :: (a -> b) -> f a -> f b //fmap takes a function that can
    // be applied any *parameterized* structure such as list or a tree
-}


data MayB a = MayB a | Nutting deriving (Show, Eq, Ord)
instance Functor MayB where
    -- fmap (a -> b) -> f a -> f b
    fmap f Nutting = Nutting
    fmap f (MayB a) = MayB (f a)
instance Applicative MayB where
    MayB f <*> Nutting = Nutting
    MayB f <*> MayB a = fmap f (MayB a)
    --(MayB f) (<*>) (MayB a) (<*>) (MayB b) = f a b
    pure a = MayB a
instance Monad MayB where
    return = pure
    MayB a >>= f = f a
    Nutting >>= f = Nutting

gv :: MayB a -> a
gv (MayB x) = x

fibby = 1: 1: [x + y | x <- fibby, y <- (tail fibby)]

facty n 
    | n <= 1 = 1
    |otherwise = n * facty (n - 1)

-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Ord)
-- instance Functor Tree where
--     fmap f (Leaf a) = Leaf (f a)
--     fmap f (Node (Leaf a) (Leaf b)) = Node (Leaf (f a)) (Leaf (f b))

-- t :: Tree Int
-- t = Node (Leaf 1) (Leaf 2)

-- Nat is the name of the type and Zero or Succ is the value constructor
-- Zero takes no arguments and Succ takes an object of type Nat as an argument
-- and returns and object of type Nat
data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

iszero :: Nat -> Bool
iszero Zero = True
iszero _ = False

-- plus :: Nat -> Nat -> Nat
-- plus m Zero = m
-- plus m (Succ n) = Succ (plus m n)

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

-- mult :: Nat -> Nat -> Nat
-- mult _ Zero = Zero
-- mult m (Succ n) = plus m (mult m n)






-- plus (Succ (Succ Zero)) (Succ (Succ Zero)) 
-- Succ (Succ (Succ (Succ Zero)))


-- iszero :: Nat -> Bool
-- iszero Zero = True
-- iszero _ = False

-- plus :: Nat -> Nat -> Nat
-- plus m Zero = m
-- plus m (Succ n) = Succ (plus m n)

-- int2nat :: Int -> Nat
-- int2nat 0 = Zero
-- int2nat n = Succ (int2nat (n - 1))

-- nat2int :: Nat -> Int
-- nat2int Zero = 0
-- nat2int (Succ n) = 1 + nat2int n



{- 
What the heck? What is this useless operator? 
It's just function application! Well, almost, but not quite! 
Whereas normal function application (putting a space between two things) 
has a really high precedence, the $ function has the lowest precedence. 
Function application with a space is left-associative (so f a b c is the same as 
((f a) b) c)), function application with $ is right-associative.
f (g (z x)) is equal to f $ g $ z x
-}

-- sum (filter (> 10) (map (*2) [2..10])) as 
-- sum $ filter (> 10) $ map (*2) [2..10]

-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Ord)

plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ n) = Succ (plus m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ Zero) = m
mult m (Succ n) = plus (mult m n) m

--main :: IO ()
--main = putStrLn "Hello, world againsfsf!"
--main = putStrLn (show (eval (Mult (Add (Val 5) (Val 6)) (Add (Val 10) (Val 20)))))
--main = putStrLn show ((mult (Succ (Succ Zero)) (Succ Zero)))
--main = putStrLn (show $ nat2int (mult (Succ (Succ (Zero))) (Succ (Succ (Succ (Succ Zero))))))

test :: IO ()
test = do
    a <- getLine
    b <- getLine
    putStrLn( a ++ b)

blast :: IO Int
blast = do
    putStrLn ("Enter first num: ")
    a <- getLine
    putStrLn ("Enter second num: ")
    b <- getLine
    return ((read b :: Int) + (read a :: Int))

slast :: IO Int
slast = do
    putStrLn ("Enter first num: ")
    a <- getLine
    putStrLn ("Enter second num: ")
    b <- getLine
    putStr ("Sum of two numbers " ++ a ++ " and " ++ b ++ " is: ")
    return ((read b :: Int) + (read a :: Int))


data Btree a = Empty | Node a (Btree a) (Btree a) deriving (Eq, Ord)
insert :: (Ord a) => a -> Btree a -> Btree a
insert v Empty = Node v (Empty) (Empty)
insert v (Node a lt rt)
    | v == a = Node v lt rt
    | v < a = Node a (insert v lt) rt
    | v > a = Node a lt (insert v rt)

isthere :: (Ord a) => a -> Btree a -> Bool
isthere v Empty = False
isthere v (Node a lt rt)
    | v == a = True
    | v < a = isthere v lt
    | v > a = isthere v rt

-- Function to print the tree with proper indentation
printTree :: (Show a) => Btree a -> String
printTree tree = unlines (go tree 0)
  where
    go Empty _ = []
    go (Node x left right) indent =
      (replicate indent ' ' ++ show x) :
      (go left (indent + 2)) ++
      (go right (indent + 2))

getht :: Btree a -> Int
getht Empty = 0
getht (Node _ left right) =
    1 + max (getht (left)) (getht (right))


printNice :: (Show a) => Btree a -> String
printNice tree = unlines (go tree 0)
    where
        go Empty _ = []
        go (Node x left right) indent =
            (replicate ((getht tree) * 4) ' ' ++ show x) :
            (go left (indent - 10)) ++
            (go right (indent + 10))

prntNice :: IO ()
prntNice = do
  let tree = foldr insert Empty [5,3,7,1,4,9]
  putStrLn (printNice tree)



prnt :: IO ()
prnt = do
  let tree = foldr insert Empty [5,3,7,1,4,9]
  putStrLn (printTree tree)


instance (Show a) => Show (Btree a) where
    show Empty = ""
    show tree = show' tree 0 (widestElement tree + 1)

show' :: (Show a) => Btree a -> Int -> Int -> String
show' Empty _ _ = " "
show' (Node a left right) depth width =
    leftside ++ "\n" ++ center ++ rightside
    where center    = replicate depth ' ' ++ show a
          leftside  = show' left (depth + width) width
          rightside = show' right (depth + width) width

widestElement :: (Show a) => Btree a -> Int
widestElement Empty = 0
widestElement (Node center left right) = maximum [l, r, c]
    where l = widestElement left
          r = widestElement right
          c = length $ show center

data Xpr = Value Int | Div Xpr Xpr deriving (Show, Eq, Ord)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Prelude.Nothing
safediv x y = Just (x `div` y)

ewal :: Xpr -> Maybe Int
ewal (Value x) = Just x
ewal (Div x y) = ewal x >>= \mx -> ewal y >>= \my -> safediv mx my

ewdo :: Xpr -> Maybe Int
ewdo (Value x) = Just x
ewdo (Div x y) = do 
                     m <- ewal x
                     n <- ewal y
                     safediv m n
                    

{-
>>= :: Maybe a -> (Maybe a -> Maybe b) -> Maybe b
mx >>= f = case mx of 
            Nothing -> Nothing
            Just mx -> f x
-}


-- type Counter = StateT Int IO

-- incrementCounter :: Counter ()
-- incrementCounter = do
--   n <- get
--   put (n + 1)

-- stater :: IO ()
-- stater = do
--   (result, count) <- runStateT incrementCounter 0
--   putStrLn $ "Result: " ++ show result
--   putStrLn $ "Count: " ++ show count

addone :: (Num a) => a -> a
addone a = a + 1

addtwo :: (Num a) => a -> a
addtwo a = addone $ addone a
