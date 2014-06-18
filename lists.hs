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

-- 7

