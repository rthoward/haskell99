-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt xs l = last $ take l xs

-- 4
myLength :: [a] -> Int
myLength xs = measure 1 xs
   where measure l (x:[]) = l
         measure l (x:xs) = measure (l + 1) xs

-- 5
myReverse :: [a] -> [a]
myReverse xs = rev xs []
   where rev [] list = list
         rev (y:ys) list = rev ys (y:list)

-- 6
isPalindrome :: String -> Bool
isPalindrome str = str == (reverse str)

-- 7 - 13 too tough for now

-- 14
-- hopefully concatMap isn't cheating, but
-- I'm getting tired of tail recursion
dupli :: [a] -> [a]
dupli l = concatMap double l
   where double x = [x, x]

-- 15
-- avoided concatMap on this one, since using
-- replicate seemed a little too obvious
repli :: [a] -> Int -> [a]
repli list times = concatMap (fill times []) list
   where fill 0 l xs = l
         fill t [] x = fill (t - 1) [x] x
         fill t l x  = fill (t - 1) (x:l) x

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery list x = zipFilter zFilter list (filterList (length list) x)
   where zipFilter filt (a:as) (b:bs)
            | filt a b  = a : zipFilter filt as bs
            | otherwise = zipFilter filt as bs
         zipFilter _    _      _ = []
         zFilter x y = y
         filterList size d = [(x `mod` d /= 0) | x <- [1..size]]

-- 17
split :: [a] -> Int -> ([a], [a])
split l x = (take x l, drop x l)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice l x y = drop (x - 1) $ take y l

