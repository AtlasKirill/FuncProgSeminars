--21--
--a)
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
--b)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs
--c)
and' :: [Bool] -> Bool
and' xs = foldl (\acc x -> acc && x) True xs
--d)
or' :: [Bool] -> Bool
or' xs = foldl (\acc x -> acc || x) False xs
--e)
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldl (&&) True . map f
--f)
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (||) False . map f
--22--
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x acc -> if f x then x : acc else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f p@(x:xs) 
	| f x = dropWhile' f xs
	| otherwise = xs
--23--
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [x] = x
foldr1' f (x:xs) = foldr f x xs
--24--
lmax :: Ord a => [a] -> a
lmax [] = error "empty list"
lmax xs = foldr1 (\x acc -> if x < acc then acc else x) xs
--25--
scanl' _ acc [] = [acc]
scanl' f acc (x:[]) = acc : [f acc x]
scanl' f acc (x:xs) = acc : (scanl' f res xs)
                            where res = f acc x
--26--
prefix :: [Char] -> [[Char]]
prefix = foldr (\el acc -> [] : map (el:) acc) [[]] 
--27--
--a)
shiftList :: [a] -> [a]
shiftList [] = []
shiftList (x:xs) = xs ++ [x]

rotts :: [a] -> [[a]]
rotts [] = [[]]
rotts list = take len (iterate shiftList list)
	where
	len = length list
--b)
rotts' :: [a] -> [[a]]
rotts' list = scanl zipAndShift list (take len (repeat list))
	where
	len = length list - 1
	zipAndShift = (\acc x -> shiftList acc)
--28--
--a)
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' (x:xs) = (fst x : map fst xs, snd x : map snd xs)
--b)
unzip'' :: [(a,b)] -> ([a],[b])
unzip'' [] = ([],[])
unzip'' list = foldr extract ([],[]) list
	where
	extract = (\el acc -> (fst el : fst acc, snd el : snd acc))
--29--
--a)
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x -> \y -> f (x,y)
--b)
uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x, y) = f x y
--30--
g :: a -> b -> d
h :: d -> c
f = curry (h . (uncurry g))
--31--
zapp :: [a -> b] -> [a] -> [b]
zapp funcs args = []
--32--
--a--
compressor [] = []
compressor xs = (head xs, num) : compressor (drop num xs)
                    where num = countFirst xs
                          countFirst xs = length $ takeWhile (head xs ==) xs
--b--
decompressor [] = []
decompressor (x:xs) = replicate (snd x) (fst x) ++ decompressor xs
