module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe (isJust)

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
    r <- randomIO :: IO Float
    return $ rulesApply $ (map . map2) (id, pick r) brain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = try . transformationsApply "*" reflect

reflectOne :: String -> String
reflectOne = try $ flip lookup reflections

reflect :: Phrase -> Phrase
reflect = map reflectOne

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = (map . map2) (f, map f)
    where f = words . map toLower


--------------------------------------


reductions :: [PhrasePair]
reductions = (map . map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc [] s = []
substitute wc (t:ts) s
    | t == wc   = s ++ substitute wc ts s
    | otherwise = t :  substitute wc ts s


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc [] [] = Just []
match wc p []  = Nothing
match wc [] s  = Nothing
match wc pl@(p:ps) sl@(s:ss)
    | p == wc   = singleWildcardMatch pl sl `orElse` longerWildcardMatch pl sl
    | p == s    = match wc ps ss
    | otherwise = Nothing


-- Defines the case when the rest of the list matches with
-- the rest of the pattern, i.e. the front wildcard removed.
singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (s:ss)
    | isJust $ match wc ps ss = Just [s]
    | otherwise               = Nothing


-- Defines the case when the rest of the list matches with
-- the pattern with the wildcard retained at the front. 
longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (wc:ps) (s:ss) = mmap (s:) (match wc (wc:ps) ss)


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions


-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> 
                       [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f xs (p1, p2) =
    mmap (substitute wc p2 . f) (match wc p1 xs)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> 
                        [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f dict p =
    foldl1 orElse $ map (transformationApply wc f p) dict


