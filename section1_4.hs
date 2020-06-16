import Data.List
--1--
--a)
myConst :: [([(Int,Int -> Int)], Char)]
myConst = [([(1, succ), (2, pred)], 'A')]
--b)
reorg :: [(Char,Int)] -> ([Int], [Char])
reorg [] = ([],[])
reorg ((c, i) : rest) = (i : fst (reorg rest), c : snd (reorg rest)) 
--c)
paired :: a -> (a,a)
paired x = (x,x)
--d)
tripleFunc :: (a -> b) -> a -> b
tripleFunc f = \x -> f x
--2--
--a)
-- type of e1 is (t1, t) -> (t, t1) where t1 and t could be any type like int or tuple
-- e1 "reverses" tuple it has as parameter 
--b)
--e2 has following type (t, t -> t1) -> t1
--we receive tuple with var and func and return function applied to this var 
--c)
--e3 has the same type as e2 except inversed params
--d)
--e4 is t1 -> (t1 -> t) -> t
--so it takes var and func and return function applied to first argument 
--e)
--x, y are functions but e5 cannot be constructed because it there are infinity attempt to apply function
--3--
--a)
conjunction' :: Bool -> Bool -> Bool
conjunction' True True = True
conjunction' _ _ = False  
--b)
implication' :: Bool -> Bool -> Bool
implication' True False = False
implication' _ _ = True
--c)
xor' :: Bool -> Bool -> Bool
xor' True a = not a
xor' False a = a
--d)
maj' :: Bool -> Bool -> Bool -> Bool
maj' x y z = (x && y) || (x && z) || (y && z) 
--4--
replicate' :: Int -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' (n-1) x 

repeat' :: a -> [a]
repeat' a = a : repeat' a
--5--
triplePif :: [(Int, Int, Int)]
triplePif = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..z], x^2 + y^2 == z^2, x < y]
--6--
divisors :: (Integral a) => a -> [a]
divisors n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

isAmicNums :: (Integral a) => a -> a -> Bool
isAmicNums x y = (sum (divisors x) == y) && (sum (divisors y) == x)
--7--
triangNums = [x | i <- [0..], let x = sum (take i [0..])]
--8--


