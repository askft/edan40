import Data.List

---- 2.2

-- 2.2.1 Multiplying List Elements

-- Faster
multiplyl' :: Num a => [a] -> a
multiplyl' = foldl1' (*)

-- Slower
multiplyr :: Num a => [a] -> a
multiplyr = foldr1 (*)


-- 2.2.2 Substitution

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (z:zs)
    | x == z    = y : (substitute x y zs)
    | otherwise = z : (substitute x y zs)

-- 2.2.5 Permutations
-- Cheating, if elements are orderable
isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

-- 2.3 Sets

data Set a = Set [a]
    deriving (Show)

empty :: Set a
empty = Set []

add :: Eq a => a -> Set a -> Set a
add x (Set xs)
    | x `elem` xs = Set xs
    | otherwise   = Set (x:xs)

remove :: Eq a => a -> Set a -> Set a
remove x (Set xs) = Set (xs \\ [x])

union' :: Eq a => Set a -> Set a -> Set a
union' (Set xs) (Set ys) = Set (xs `union` ys)

intersect' :: Eq a => Set a -> Set a -> Set a
intersect' (Set xs) (Set ys) = Set (xs `intersect` ys)

elem' :: Eq a => a -> Set a -> Bool
elem' x (Set xs) = x `elem` xs

isEmpty :: Eq a => Set a -> Bool
isEmpty (Set xs) = xs == []




