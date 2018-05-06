
-- Assignment 2: String Alignment
-- EDAN40 - Functional Programming
-- http://cs.lth.se/edan40/programming-assignments/string-alignment/

module StringAlignment where

-- Define the scoring system
scoreMatch    = 0
scoreMismatch = -1
scoreSpace    = -1


-- Define a few strings to test our solution on
string1 = "writers"
string2 = "vintner"
string3 = "aferociousmonadatemyhamster"
string4 = "functionalprogrammingrules"
string5 = "bananrepubliksinvasionsarmestabsadjutant"
string6 = "kontrabasfiolfodralmakarmästarlärling"


-- Return the score of the alignment of two characters
score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y    = scoreMatch
    | otherwise = scoreMismatch


-- Similarity score of optimal alignment (inefficient)
similarityScore :: String -> String -> Int
similarityScore xl [] = scoreSpace * length xl
similarityScore [] yl = scoreSpace * length yl
similarityScore xl@(x:xs) yl@(y:ys) = maximum
                                        [ similarityScore xs ys + score x y,
                                          similarityScore xs yl + score x '-',
                                          similarityScore xl ys + score '-' y ]


-- Similarity score of optimal alignment (efficient, with memoization)
similarityScoreMemo :: String -> String -> Int
similarityScoreMemo xs ys = simScore (length xs) (length ys)
    where
        simScore :: Int -> Int -> Int
        simScore i j = table !! i !! j

        table :: [[Int]]
        table = [[entry i j | j <- [0..]] | i <- [0..]]

        entry :: Int -> Int -> Int
        entry 0 0 = 0
        entry i 0 = i * scoreSpace
        entry 0 j = j * scoreSpace
        entry i j = maximum [simScore (i - 1) (j - 1) + score x y,
                             simScore (i - 1)  j      + score x '-',
                             simScore  i      (j - 1) + score '-' y]
            where
                x = xs !! (i - 1)
                y = ys !! (j - 1)


-- Prepend x and y to the first and the second list,
-- respectively, of each tuple in the list aList.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads x y list = [(x:xs, y:ys) | (xs, ys) <- list]


-- Generalized maximum function in two respects:
-- 1) The value of an element is defined by a function supplied as a parameter.
-- 2) Instead of just one element, the result is a list of all maximum elements.
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy f xs = [x | x <- xs, f x == maximum (map f xs)]


-- Define a type for a string alignment pair
type Alignment = (String, String)


-- Optimal alignments (inefficient)
optAlignments :: String -> String -> [Alignment]
optAlignments []     []     = [([], [])]
optAlignments (x:xs) []     = attachHeads x '-' $ optAlignments xs []
optAlignments []     (y:ys) = attachHeads '-' y $ optAlignments [] ys
optAlignments (x:xs) (y:ys) = maximaBy sim alignments
    where
        sim (a, b) = similarityScore a b
        alignments = concat [ attachHeads x y   $ optAlignments xs ys,
                              attachHeads x '-' $ optAlignments xs (y:ys),
                              attachHeads '-' y $ optAlignments (x:xs) ys ]


-- Optimal alignment (efficients, with memoization)
optAlignmentsMemo :: String -> String -> [Alignment]
optAlignmentsMemo xs ys = map
                          (pairApply reverse)
                          (snd $ alignment (length xs) (length ys))
    where
        pairApply f (a, b) = (f a, f b)

        alignment :: Int -> Int -> (Int, [Alignment])
        alignment i j = table !! i !! j

        table :: [[(Int, [Alignment])]]
        table = [[entry i j | j <- [0..]] | i <- [0..]]

        entry :: Int -> Int -> (Int, [Alignment])
        entry 0 0 = (0, [([], [])])
        entry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
        entry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
        entry i j = (fst $ head best, concat [snd b | b <- best])
            where 
                (s1, a1) = alignment (i - 1) (j - 1)
                (s2, a2) = alignment (i - 1)  j
                (s3, a3) = alignment  i      (j - 1)
                x = xs !! (i - 1)
                y = ys !! (j - 1)
                best = maximaBy fst $ [
                    (s1 + score x y,   attachHeads x y   a1),
                    (s2 + score x '-', attachHeads x '-' a2),
                    (s3 + score '-' y, attachHeads '-' y a3)]


-- Print all optimal alignments between s1 and s2.
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
    putStr $ "\nThere are " ++ show (length alignments) ++ " optimal alignments"
             ++ " between '" ++ s1 ++ "' and '" ++ s2 ++ "', with a similarity"
             ++ " score of " ++ show (similarityScoreMemo s1 s2) ++ ":\n\n"
    mapM_ putStrLn $ map formatPair alignments -- comment to unbloat
        where
            formatPair (a, b) = a ++ "\n" ++ b ++ "\n"
            alignments = optAlignmentsMemo s1 s2


-- Just enter the variables testN in ghci to see the results
test1 = outputOptAlignments string1 string2 -- n should be 3
test2 = outputOptAlignments string3 string4 -- n should be 308
test3 = outputOptAlignments string5 string6 -- n should be 1736

