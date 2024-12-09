periodLength :: Int -> Int
periodLength n
    | n `mod` 2 == 0 = periodLength (n `div` 2) 
    | n `mod` 5 == 0 = periodLength (n `div` 5) 
    | otherwise = findPeriod 1 n 1
  where
    findPeriod reszta mianownik counter
        | reszta == 0 = 0     
        | (10 * reszta) `mod` mianownik == 1 = counter
        | otherwise = findPeriod ((10 * reszta) `mod` mianownik) mianownik (counter + 1)

longestPeriods :: Int -> [(Int, Int)]
longestPeriods n =
    let pairs = [(k, periodLength k) | k <- [2..n]]
        maxPeriod = maximum (map snd pairs)
    in filter ((== maxPeriod) . snd) pairs

main :: IO ()
main = do
    putStrLn "----------------------------------"
    putStrLn "Zadanie 13:"
    putStrLn "Podaj liczbę n:"
    input <- getLine
    let n = read input :: Int
    let results = longestPeriods n
    putStrLn $ "Ulamki z najdluzszym okresem: " ++ show (map fst results)
    putStrLn $ "Najdluzszy okres: " ++ show (snd (head results))
    putStrLn "----------------------------------"

