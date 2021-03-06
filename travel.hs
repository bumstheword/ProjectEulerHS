import Data.List

f :: [Int] -> Int
f (x:xs)
    | length (x:xs) <= 4 = product (x:xs)
    | otherwise = max (product (take 4 (x:xs))) (f xs)

g :: [[Int]] -> Int
g xs = (maximum . map f) xs

h :: Int -> Int
h n = (+) 0 $ read $ flip (:) [] $ head $ drop (n - 1) $ concat $ map show [1..]

sum' :: [Int] -> Int
sum' (x:xs) 
    | (x:xs) == [x] = 0
    | otherwise = x + sum' xs

main = do
    print $ [(h 10),(h 100),(h 1000), (h 10000), (h 100000),(h 1000000)]
    print $ product [(h 10),(h 100),(h 1000), (h 10000), (h 100000), (h 1000000)]


isAbundant :: Int -> Bool
isAbundant n = (sum . factors) n > n

factors :: Int -> [Int]
factors n = filter (\x -> mod n x == 0) [1..(div n 2)]

isMult :: Int -> Bool
isMult n = (filter isAbundant . factors) n

main = do
    (putStrLn . show . filter isAbundant) [1..100]
    (putStrLn . show . filter isAbundant) [101..200]
    (putStrLn . show . filter isAbundant) [201..300]
    (putStrLn . show . sum . filter odd) [1..28123]
