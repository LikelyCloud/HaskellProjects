-- Implementation of the Collatz function (that applies the function to its result until 1 is reached) that also returns trace of computations
  
collatz :: (Integral a) => a -> a
collatz n = if even n then n `div` 2 else 3 * n + 1

{-
λ> collatz 1
4
λ> collatz 2
1
λ> collatz 4
2
-}

collatzHelper :: (Integral a, Show a) => a -> [a] -> Writer [String] [a]
collatzHelper n xs
  | n == 1 = do
      tell ["Koniec!"]
      return (reverse xs)
  | otherwise = do
      tell ["collatz " ++ show n ++ " = " ++ show (collatz n)]
      collatzHelper (collatz n) (collatz n : xs)

collatzWriter :: (Integral a, Show a) => a -> Writer [String] [a]
collatzWriter n = collatzHelper n [n]

{-
λ> runWriter (collatzWriter 1024)
([1024,512,256,128,64,32,16,8,4,2,1],["collatz 1024 = 512","collatz 512 = 256","collatz 256 = 128","collatz 128 = 64","collatz 64 = 32","collatz 32 = 16","collatz 16 = 8","collatz 8 = 4","collatz 4 = 2","collatz 2 = 1","Koniec!"])

λ> runWriter (collatzWriter 1025)
([1025,3076,1538,769,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1],["collatz 1025 = 3076","collatz 3076 = 1538","collatz 1538 = 769","collatz 769 = 2308","collatz 2308 = 1154","collatz 1154 = 577","collatz 577 = 1732","collatz 1732 = 866","collatz 866 = 433","collatz 433 = 1300","collatz 1300 = 650","collatz 650 = 325","collatz 325 = 976","collatz 976 = 488","collatz 488 = 244","collatz 244 = 122","collatz 122 = 61","collatz 61 = 184","collatz 184 = 92","collatz 92 = 46","collatz 46 = 23","collatz 23 = 70","collatz 70 = 35","collatz 35 = 106","collatz 106 = 53","collatz 53 = 160","collatz 160 = 80","collatz 80 = 40","collatz 40 = 20","collatz 20 = 10","collatz 10 = 5","collatz 5 = 16","collatz 16 = 8","collatz 8 = 4","collatz 4 = 2","collatz 2 = 1","Koniec!"])
-}
