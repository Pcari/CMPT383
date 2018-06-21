import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys)
  | x < y = x: merge xs (y:ys)
  | otherwise = y: merge (x:xs) ys

hailLen :: Int -> Int
hailLen n = hailTail 0 n
  where
    hailTail a 1 = a
    hailTail a n = hailTail (a+1) (hailstone n)



--hailLen :: Int -> Int
--hailLen 1 = 0
--hailLen n = 1 + hailLen (hailstone n)

hailstone n
  | even n  = n `div` 2
  | otherwise = (3 * n) + 1


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fact' :: Int -> Int
fact' n = foldl (*) 1 [1..n]


daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31


weekday :: Day -> Int
weekday = snd . mondayStartWeek


isFriday :: Day -> Bool
isFriday d 
  | weekday d == 5 = True
  | otherwise = False


divisors :: Int -> [Int]
divisors n = [ i | i <- [1..n], n `mod` i == 0 ]

divCount :: Int -> Int
divCount = length . divisors

isPrime :: Int -> Bool
isPrime n = divCount n == 2  

getDay (y,m,d) = d

isPrimeDay :: Day -> Bool
isPrimeDay d = isPrime $ getDay $ toGregorian d




--primeFridays :: Int -> [[Day]]
primeFridays n = [i | i <- daysInYear n, (isFriday i && isPrimeDay i) ]














