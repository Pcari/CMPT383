import Data.List
import Data.Maybe

hailstone n
  | even n  = n `div` 2
  | otherwise = (3 * n) + 1

hailSeq :: Integer -> [Integer]
hailSeq 1 = [1]
hailSeq n = n : hailSeq (hailstone n)

hailSeq' :: Integer -> [Integer]
hailSeq' n = (takeWhile (/=1) $ iterate hailstone n) ++ [1]

join:: [Char] -> [[Char]] -> [Char]
join str [] = []
join str [x] = x
join str xs = foldl1 (\x acc -> x ++ str ++ acc) xs

--join str xs = foldr (\x acc -> x ++ str ++ acc) "" xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys)
  | x < y = x: merge xs (y:ys)
  | otherwise = y: merge (x:xs) ys


firstHalf :: [a] -> [a]
firstHalf xs = take (length xs `div` 2) xs

secondHalf :: [a] -> [a]
secondHalf xs = drop (length xs `div` 2) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort [] = []
mergeSort lst = merge (mergeSort (firstHalf lst)) (mergeSort (secondHalf lst))

findElt :: Eq a => a -> [a] -> Maybe Int
findElt x xs = elemIndex x xs

