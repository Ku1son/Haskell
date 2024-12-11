import Data.List (sort,subsequences,nub)
import Data.List (sortBy)
import Data.Ord (comparing)
-- Sprawdza, czy liczba jest palindromem
isPalindrome :: Int -> Bool
isPalindrome x = let s = show x in s == reverse s

-- Sprawdza, czy liczba może być zapisana jako suma kwadratów kolejnych liczb naturalnych
isSumOfSquares :: Int -> Bool
isSumOfSquares x = not (null (findSquares x))

-- Znajduje wszystkie kombinacje sum kwadratów dla liczby x
findSquares :: Int -> [[Int]]
findSquares x = [take n [k..] | k <- [1..maxElem], n <- [1..maxElem], sum (map (^2) (take n [k..])) == x]
  where
    maxElem = floor . sqrt . fromIntegral $ x

-- Znajduje wszystkie palindromy mniejsze od n, które spełniają warunek
findPalindromes :: Int -> [Int]
findPalindromes n = filter (\x -> isPalindrome x && isSumOfSquares x) [1..(n-1)]

-- Oblicza długość okresu w rozwinięciu dziesiętnym liczby n
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

-- Znajduje liczby z najdłuższym okresem w rozwinięciu dziesiętnym
longestPeriods :: Int -> [(Int, Int)]
longestPeriods n =
    let pairs = [(k, periodLength k) | k <- [2..n]]
        maxPeriod = maximum (map snd pairs)
    in filter ((== maxPeriod) . snd) pairs



-- Funkcja generująca wszystkie możliwe ciągi cyfr od 0 do m, gdzie każda cyfra może wystąpić maksymalnie p razy,
-- oraz przynajmniej jeden ciąg musi zawierać cyfrę '1'.
generateAllSequences :: Int -> Int -> [String]
generateAllSequences m p =
    let digits = concatMap (\d -> replicate p d) ['0'..(head (show m))] -- Każda cyfra występuje maksymalnie p razy
        allSubsequences = subsequences digits                           -- Wszystkie podciągi, dowolnej długości
    in filter (\s -> elem '1' s) allSubsequences                        -- Usuwamy ciągi bez '1'

-- Funkcja sortująca: najpierw według długości, potem według wartości liczbowej
sortSequences :: [String] -> [String]
sortSequences = sortBy ( comparing numericValue <> comparing length)

-- Funkcja obliczająca wartość liczbową ciągu z wiodącymi zerami
numericValue :: String -> Int
numericValue = read

  
-- Funkcja zwracająca podciągi do pozycji n, bez powtórzeń
subsequencesUpToN :: Int -> Int -> Int -> [String]
subsequencesUpToN n m p =
    let allSequences = generateAllSequences m p -- Wszystkie możliwe ciągi spełniające warunek
        uniqueSequences = nub allSequences      -- Usunięcie powtarzających się podciągów
        sortedSequences = sortSequences uniqueSequences -- Posortowane najpierw według długości, potem liczbowo
    in take n sortedSequences                   -- Pobieramy ciągi aż do n-tego
    
-- Funkcja zwracająca n-ty ciąg (indeks zaczyna się od 1) z posortowanej listy podciągów
subsequenceNth :: Int -> Int -> Int -> String
subsequenceNth n m p =
    let allSequences = generateAllSequences m p -- Wszystkie możliwe ciągi spełniające warunek
        uniqueSequences = nub allSequences      -- Usunięcie powtarzających się podciągów
        sortedSequences = sortSequences uniqueSequences -- Posortowane najpierw według długości, potem liczbowo
    in sortedSequences !! (n - 2)  -- Zwracamy n-ty ciąg 
    
-- Główna funkcja programu
main :: IO ()
main = do

    
    
    -- Zadanie 35
    let n = 4
    let m = 2
    let p = 2
    --mapM_ print $ subsequencesUpToN n m p
    putStrLn $ subsequenceNth n m p
    
    -- Zadanie 1
    putStrLn "Podaj wartość n dla palindromów:"
    input26 <- getLine
    let n = read input26 :: Int
    let palindromes = findPalindromes n
    putStrLn "Palindromy mniejsze od n, które są sumą kwadratów kolejnych liczb naturalnych:"
    mapM_ print palindromes

    -- Przerwa wizualna
    putStrLn "----------------------------------"

    -- Zadanie 2
    putStrLn "Podaj wartość n dla liczb z najdłuższym okresem:"
    input13 <- getLine
    let n2 = read input13 :: Int
    let results = longestPeriods n2
    putStrLn $ "Liczby z najdłuższym okresem: " ++ show (map fst results)
    putStrLn $ "Najdłuższy okres: " ++ show (snd (head results))
    putStrLn "----------------------------------"
    
    
    
