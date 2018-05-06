
-- Definitions
scoreMatch    = 0
scoreMismatch = -1
scoreSpace    = -1
string1 = "writers"
string2 = "vintner"
string3 = "aferociousmonadatemyhamster"
string4 = "functionalprogrammingrules"
string5 = "bananrepubliksinvasionsarmestabsadjutant"
string6 = "kontrabasfiolfodralmakarmästarlärling"


score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y    = scoreMatch
    | otherwise = scoreMismatch


-- Return the score of the optimal alignment of the two strings s1 and s2.
-- If you need to, consult the Hint section below.
similarityScore :: String -> String -> Int
similarityScore xl [] = scoreSpace * length xl
similarityScore [] yl = scoreSpace * length yl
similarityScore xl@(x:xs) yl@(y:ys) = maximum
                                        [ similarityScore xs ys + score x y,
                                          similarityScore xs yl + score x '-',
                                          similarityScore xl ys + score '-' y ]


-- Explain what this function does!
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]


-- Generalized maximum function in two respects:
-- 1 The "value" of an element is defined by a function supplied as a parameter.
-- 2 Instead of just one element, the result is a list of all maximum elements.
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy f xs = [x | x <- xs, f x == maximum (map f xs)]


type Alignment = (String, String)


-- Return a list of all optimal alignments between s1 and s2.
-- (Hint: Follow the same pattern as you did in part a., and make use of the
-- functions defined in parts b. and c.)
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

type Index = Int
type Score = Int

-- Optimal Alignment with Memoization
oam :: String -> String -> [Alignment]
oam xs ys = (map . pairApply) reverse (snd $ alignment (length xs) (length ys))
    where
        pairApply f (a, b) = (f a, f b)

        alignment :: Int -> Int -> (Int, [Alignment])
        alignment i j = table !! i !! j

        table :: [[(Int, [Alignment])]]
        table = [[entry i j | j <- [0..]] | i <- [0..]]

        entry :: Int -> Int -> (Int, [Alignment])
        entry 0 0 = (0, [([], [])])
--        entry i 0 = ...
--        entry 0 j = ...
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




-- Print all optimal alignments between s1 and s2 to the screen in a
-- neat and easy-to-read fashion.
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
    putStrLn $ "Optimal alignments between " ++ s1 ++ " and " ++ s2 ++ " are:\n"
    putStr $ concatMap (\(a,b) -> a ++ "\n" ++ b ++ "\n\n") $ oam s1 s2
--    pairApply (mapM_ putStrLn) (oam s1 s2)

-- pairApply f (a, b) = (f a, f b)

main = outputOptAlignments string1 string2


