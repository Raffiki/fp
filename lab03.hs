module Lab03 where


import Data.Char
swap :: [(a, b)] -> [(b, a)]
swap ts = [(b, a) | (a, b) <- ts]


concat1 :: [[a]] -> [a]
concat1 xxs = [x | xs <- xxs, x <- xs ] 
    
keepVowels :: [String] -> [String]
keepVowels = map (filter (\l -> elem l ['a', 'e']))
-- keepVowels words = [filter (\l -> elem l ['a', 'e']) w |w <- words]

doubleMap :: (a -> b) -> [[a]] -> [[b]]
doubleMap f = map (map f)
-- doubleMap f xss = [ map f xs | xs <- xss]

isPalindrome :: String -> Bool
isPalindrome  str = s == (reverse s) where 
    s = map toUpper (filter isLetter str)

rotate :: [a] -> [a]
rotate [] = []
rotate (h:t) = t ++ [h]

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

circumference :: [(Double,Double)] -> Double
circumference crs = sum $ zipWith distance crs $ rotate crs

valu :: [Int] -> Int
valu xs = foldr