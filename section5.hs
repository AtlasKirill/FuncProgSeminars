--9--
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n x
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: (Num i, Ord i) => i -> [a] -> [a] 
drop' _ [] = []
drop' n x@(_:xs)
	| n > 0 = drop' (n - 1) xs
	| otherwise = x

null' :: [a] -> Bool
null' [] = True
null' (_:_) = False

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = if x == a then True else elem' a xs

-- (!!) implementation
pos :: Int -> [a] -> a
pos _ [] = error "Empty list"
pos n (x:xs)
	| n > 0 = pos (n - 1) xs
	| otherwise = x

-- (++) implementation
conc' :: [a] -> [a] -> [a]
conc' [] y = y
conc' (x:xs) y = x : conc' xs y

--concat implementation
concat' :: [[a]] -> [a]
concat' [] = []
concat' ([]:xss) = concat' xss
concat' ((x:xs):xss) = x : concat' (xs : xss )
--10--
help :: a -> [a] -> [[a]]
help x [] = [[x]]
help x all@(y:ys) = [x:all] ++ help x (init all)

segs :: [a] -> [[a]]
segs [] = []
segs (x:xs) =(help x xs) ++ segs xs
--11--
--a)
gcd1 :: Integral a => a -> a -> a
gcd1 0 0 = error "GCD isn't defined for 0 0"
gcd1 x y = gcd' (abs x) (abs y)
	where gcd' x 0 = x
	      gcd' 0 x = x
	      gcd' x y
                     | x < y = gcd' (y `mod` x) x
                     | otherwise = gcd' (x `mod` y) y
--b)
gcd2 :: Integral a => a -> a -> a
gcd2 0 0 = error "GCD isn't defined for 0 0"
gcd2 x y = gcdHelp (abs x) (abs y)
	where gcdHelp x 0 = x
	      gcdHelp x y 
			| x < y = gcdHelp (y - x) x
        		| otherwise = gcdHelp (x - y) y 
--12--
gcdExt :: Integral a => a -> a -> (a,a,a)
gcdExt 0 b = (b, 0, 1)
gcdExt a b = let (g, s, t) = gcdExt (b `mod` a) a
           in (g, t - (b `div` a) * s, s)
--13--
noGcd (_, a, b) = (a, b)
eqSolve :: Int -> Int -> Int -> [(Int, Int)]
eqSolve a b c | mod c d /= 0 = []
                    where d = gcd a b
eqSolve a b c = let d = gcd a b
                    a0 = div a d
                    b0 = div b d
                    c0 = div c d
                    (u0, v0) = noGcd (gcdExt a0 b0) 
                    u = u0 * c0
                    v = v0 * c0
                 in (u,v) : concat [[(u - b0 * t, v + a0 * t),(u - b0 * (-t), v + a0 * (-t))] | t <- [1..5]]
--14--
dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = x : x : dupl xs
--15--
nrem :: Int -> [a] -> [a]
nrem n l = [ f | (f, s) <- zip l [1..], s `mod` n /= 0]
--16--
repRem :: Eq a => [a] -> [a]
repRem [] = []
repRem (x:xs) = x : repRem (filter (/=x) xs)
--17--
findKthMin n (x:xs) = if n == (l + 1)
                      then x
                      else if n <= l
                           then findKthMin n lesser
                           else findKthMin (n-l-1) greater
                      where lesser = [y | y <- xs, y < x]
                            greater = [y | y <- xs, y > x]
                            l = length lesser
--18--
part :: Int -> Int -> [[Int]]
part 0 _ = [[]]
part 1 n = [[n]]
part m n = concat [map (i:) (part (m-1) (n-i)) | i <- [0..n]]
--19--
sbseqs :: [a] -> [[a]]
sbseqs [] = [[]]
sbseqs xs = concat [sbseqsl l xs | l <- [1..(length xs)]]

sbseqsl n xs | n > (length xs) = [[]]
sbseqsl n xs | n == (length xs) = [xs]
sbseqsl 1 xs = [[x] | x <- xs]
sbseqsl n (x:xs) = (map (x:) (sbseqsl (n-1) xs)) ++ (sbseqsl n xs)






