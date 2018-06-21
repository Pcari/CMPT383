hailstone n
  | even n  = n `div` 2
  | otherwise = (3 * n) + 1

hailLen :: Int -> Int
hailLen 1 = 0
hailLen n = 1 + hailLen (hailstone n)

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]


primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

join :: [a] -> [[a]] -> [a]
join str [x] = x
join str [] = []
join str (x:xs) = x ++ str ++ (join str xs)

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a,b,c) | a <- [1..n], b <-[1..a], c <-[1..n], ((a^2)+(b^2) == (c^2))]