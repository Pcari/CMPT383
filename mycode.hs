import System.Random

half x = x/2
isBig a = a > 100
listify x y = [x, y]

myAnd False _ = False
myAnd True a = a

isZero 0 = True
isZero _ = False

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

listEqual [] [] = True
listEqual (x:xs) (y:ys) = x == y && listEqual xs ys
listEqual _ _ = False

mySignum x
  | x>0         = 1
  | x<0         = -1
  | otherwise   = 0

-- cases
word n = case n of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  _ -> "???"

wordWithX n = (case n of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  _ -> "???") ++ "X"

describeList lst = "The list is " ++ case lst of
  _:_:_:_:_  -> "fairly long"
  _:_        -> "short"
  []         -> "empty"

firstSquares n = [ i*i | i <- [1..n] ]
firstEvenSquares n = [ i*i | i <- [1..n], even i]
firstEvenSquares' n = [ i*i | i <- [2,4..n]]

qsort [] = []
qsort (x:xs) = smaller ++ [x] ++ larger
  where smaller = qsort [a | a <- xs, a<=x]
        larger = qsort [a | a <- xs, a>x]

qsort' [] = []
qsort' (x:xs) =
  let smaller = qsort' [a | a<-xs, a<=x]
      larger = qsort' [a | a<-xs, a>x]
  in smaller ++ [x] ++ larger

somePowers x = [x, sq x, sq (sq x)]
  where sq n = n*n

myPower _ 0 = 1
myPower x y = x * myPower x (y-1)

--myConcat xs = foldl (++) [] xs

myConcat :: [[a]] -> [a]
myConcat xs = foldl (++) [] xs

myConcat' :: [[a]] -> [a]
myConcat' = foldl (++) []

myReverse xs = foldl prefix [] xs
    where prefix xs x = x:xs

myReverse' xs = foldr postfix [] xs
    where postfix x xs = xs ++ [x]

hailstone n
  | even n  = n `div` 2
  | otherwise = (3 * n) + 1

hailLen :: Int -> Int
hailLen 1 = 0
hailLen n = 1 + hailLen (hailstone n)

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

divisors n = filter (divides n) [2..(n `div` 2)]
  where divides a b = (a `mod` b) == 0


primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

join :: [a] -> [[a]] -> [a]
join str [x] = x
join str [] = []
join str (x:xs) = x ++ str ++ (join str xs)

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a,b,c) | a <- [1..n], b <-[1..a], c <-[1..n], ((a^2)+(b^2) == (c^2))]

addPairwise :: Num a => [a] -> [a] -> [a]
addPairwise xs ys = map (uncurry (+)) (zip xs ys)

--hailLen :: Int -> Int
--hailLen = length . hailSeq

reverseJoin :: String -> [String] -> String
reverseJoin s = (join s) . reverse

--weekday :: Day -> Int
--weekday = snd . mondayStartWeek

joinPrimes :: String -> String
joinPrimes = (flip join) ["2", "3", "5", "7", "11"]

myReverse'' :: [a] -> [a]
myReverse'' xs = foldl (flip (:)) [] xs

addToEach :: Num a => a -> [a] -> [a]
addToEach n lst = map (\x -> x+n) lst

threeRand' :: IO [Int]
threeRand' = do
    gen0 <- newStdGen
    let
        (rand0, gen1) = randomR (1, 100) gen0
        (rand1, gen2) = randomR (1, 100) gen1
        (rand2, _)    = randomR (1, 100) gen2
    return [rand0, rand1, rand2]


threeRand :: [Int]
threeRand = 
    let gen0 = mkStdGen 1234 -- gen0 :: StdGen
        (rand0, gen1) = randomR (1, 100) gen0
        (rand1, gen2) = randomR (1, 100) gen1
        (rand2, _)    = randomR (1, 100) gen2
    in [rand0, rand1, rand2]

threeRand'' :: IO [Int]
threeRand'' = do
    gen0 <- newStdGen
    return $ take 3 $ randomRs (1, 100) gen0


-- generate n random integers
randInts :: Int -> Int -> Int -> IO [Int]
randInts n minval maxval = do
    gen <- newStdGen
    return $ take n $ randomRs (minval, maxval) gen

-- convert a list of values into a histogram
histogram :: (Enum a, Eq a, Ord a) => [a] -> [String]
histogram vals = bars
    where
    counts = [length $ filter (==i) vals
             | i <- [(minimum vals)..(maximum vals)]]
    bars = [take n $ repeat 'X' | n <- counts]

-- print histogram of randomly-generated values
printHisto :: IO ()
printHisto = do
    vals <- randInts 1000 1 20
    let bars = histogram vals
    mapM_ putStrLn bars