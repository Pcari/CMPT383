
pascal:: Integer -> [Integer]
pascal 0 = [1]
pascal 1 = [1,1]
pascal n = 	[1] ++ [x + y | (x,y) <- add (pascal (n-1))] ++ [1]
  where add (x:y:xs) = (x,y) : add (y:xs)
        add _ = []

addPair :: Num a => (Integer, Integer) -> Integer
addPair = uncurry (+)

withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros [] = []
withoutZeros xs = [x | x <- xs, x /= 0]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs = map fib [0..]

things :: [Integer]
things = 0 : 1 : zipWith (+) things (tail things)