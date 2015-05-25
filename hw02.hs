{-# OPTIONS_GHC -Wall #-}
module HW02 where
-- Mastermind -----------------------------------------

import Data.List
import Data.Ord
-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter (==True) $  zipWith equal a b 
  where equal :: Peg -> Peg -> Bool
        equal x y = (x==y)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\x -> length . filter (==x) $ xs ) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith (min) (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove a b = Move b enum (mnum-enum)
  where enum = exactMatches a b
        mnum = matches a b

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move xs a b) ys = (Move xs a b) == (getMove ys xs)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m xs = filter (\x -> isConsistent m x) xs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]] 
allCodes n = concatMap (\x -> map (x:) (allCodes (n-1))) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = guess (allCodes 4)
  where guess :: [Code] -> [Move]
        guess [] = []
        guess (x:[]) = [getMove secret x]
        guess (x:xs) = (getMove secret x) : guess ( filterCodes move xs )
          where move = getMove secret x
  
-- Bonus ----------------------------------------------

getCode :: Move -> Code
getCode (Move c _ _) = c

getColors :: Move -> (Int,Int)
getColors (Move _ a b) = (a,b)

matchesEqual :: Move->Move->Bool
matchesEqual (Move _ a b) (Move _ a' b') = (a==a') && (b==b')

countEliminated :: [Code]->Move->Int
countEliminated xs m = length xs - length (filterCodes m xs)

possibleScoresWithMove :: [Code] -> Code -> [Move]
possibleScoresWithMove xs x = nubBy matchesEqual $ map (getMove x) xs

getScore :: [Code] -> Code -> Int
getScore xs x = minimum $ map (countEliminated xs) (possibleScoresWithMove xs x)

firstCode :: Code
firstCode = [Red,Red,Green,Green]

breakSecrets :: [Move] -> (Move,Int)
breakSecrets [] = (Move [] 0 0,0) 
breakSecrets (x:xs) = (x,1+length xs)

classifySecrets :: [Code] -> Code -> [(Move,Int)]
classifySecrets xs g = map (\x->(fst x, length xs - snd x) ) $ map breakSecrets $ groupBy matchesEqual $ sortBy (comparing getColors) $ map (getMove g) xs

getGuessScore :: [Code] -> Code -> Int
getGuessScore xs g = (length xs) - (maximum $ map length $ groupBy matchesEqual $ sortBy (comparing getColors) $ map (getMove g) xs)

cs :: [Code]
cs = allCodes 4

nextCode :: [Code] -> [Move] -> Code
nextCode [] _ = []
nextCode (x:[]) _ = x
nextCode _ [] = [Red,Red,Green,Green]
nextCode xs ys = maximumBy (comparing (getGuessScore xs)) (cs \\ (map getCode ys))


fiveGuess :: Code -> [Move]
fiveGuess secret = guess cs []
  where guess :: [Code] -> [Move] -> [Move]
        {-guess xs b | trace("Move:" ++ show b ++ "Remaining:" ++ show (length xs)) False = undefined-}
        guess [] xs = reverse xs
        guess (x:[]) xs = guess [] (getMove secret x:xs)
        guess xs ys = guess (filterCodes nextMove xs) (nextMove:ys) 
          where nextMove = getMove secret $ nextCode xs ys
        {-guess (x:[]) xs  = guess [] ((getMove secret x):xs)-}
        {-guess _ [] = guess (filterCodes firstMove cs) [firstMove]-}
          {-where firstMove = getMove secret firstCode -}
        {-guess xs ys = guess (filterCodes nextMove xs) (nextMove:ys)-}
          {-where gs = cs  \\ (map getCode ys)-}
                {-nextCode = maximumBy (comparing (getGuessScore xs)) gs-}
                {-nextMove = getMove secret nextCode-}



