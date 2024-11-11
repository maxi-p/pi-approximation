-- Author: Maksim Petrushin
-- PID (UCFID): 5494613
-- Assignment #2: Approximating PI using different formulas
-- Compiler: https://play.haskell.org/
-- Accuracy Asnwer: Wells Pi is the most accurate at k=200 (aside from Short Pi)

-- Short pi function is non-recursive
shortPi :: Double
shortPi = 
  4 * (4 * atan(0.2::Double) - atan(1 / 239))

-- It doesn't matter in which order we do addition. 
-- This recurcive function does addition from k to 1
simplePiInner :: Int -> Double -> Double
simplePiInner 0 acc = acc
simplePiInner k acc = do
  simplePiInner (k-1) (acc + (-1)^^(k+1) * 1 / fromIntegral (2*k-1))

-- Simple Pi wrapper function
simplePi :: Int -> Double
simplePi k = 4 * (simplePiInner k 0)

-- It doesn't matter in which order we do addition. 
-- This recurcive function does from k to 1
wellsPiInner :: Int -> Double -> Double
wellsPiInner 0 acc = acc
wellsPiInner k acc = do
  wellsPiInner (k-1) (acc + 1 / fromIntegral ((2*k-1)^2))

-- Wells Pi wrapper function
wellsPi :: Int -> Double
wellsPi k = sqrt (8 * (wellsPiInner k 0))

-- It doesn't matter in which order we do addition. 
-- This recurcive function does from k to 1
eulerPiInner :: Int -> Double -> Double
eulerPiInner 0 acc = acc
eulerPiInner k acc = do
  eulerPiInner (k-1) (acc + 1 / fromIntegral (k^2))

-- Euler Pi wrapper function
eulerPi :: Int -> Double
eulerPi k = sqrt (6 * (eulerPiInner k 0))

-- It doesn't matter in which order we do multiplications. 
--This recurcive function does multiplications from k to 1
wallisSumPiInner :: Int -> Double -> Double
wallisSumPiInner 0 acc = acc
wallisSumPiInner k acc = do
  wallisSumPiInner (k-1) (acc * fromIntegral ((2*k)^2) / fromIntegral ((2*k-1)*(2*k+1)))

-- Wallis Pi wrapper function
wallisSumPi :: Int -> Double
wallisSumPi k = 2 * (wallisSumPiInner k 1)

-- main function that prints all results
main :: IO()
main = do
  putStrLn "Simple Pi"
  putStrLn $ show (simplePi (200))
  putStrLn "Wells Pi"
  putStrLn $ show (wellsPi (200))
  putStrLn "Short Pi"
  putStrLn $ show shortPi
  putStrLn "Euler Pi"
  putStrLn $ show (eulerPi (200))
  putStrLn "WallisSum PI"
  putStrLn $ show (wallisSumPi (200))
