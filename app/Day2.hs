{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-2.txt"

  putStrLn "Part 1:"
  print $! sum . map score $ input

  putStrLn "\nPart 2:"
  print $! sum . map (score . remapRightShape) $ input

data Shape = Rock | Paper | Scissors deriving (Show)

-- | The right shape is ours.
data Round = Round Shape Shape deriving (Show)

-- | From the player's perspective.
data Outcome = Win | Loss | Draw

-- * Part 1

-- | From the player's i.e. the right hand's perspective.
score :: Round -> Int
score r@(Round _ shape) = outcomeScore (outcome r) + shapeScore shape

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Loss = 0
outcomeScore Draw = 3

-- | From the player's i.e. the right shape's perspective.
outcome :: Round -> Outcome
outcome (Round Rock Paper) = Win
outcome (Round Paper Scissors) = Win
outcome (Round Scissors Rock) = Win
outcome (Round Paper Rock) = Loss
outcome (Round Scissors Paper) = Loss
outcome (Round Rock Scissors) = Loss
outcome _ = Draw

-- * Part 2

-- | Remap the right shape according to the rules of part 2.
remapRightShape :: Round -> Round
remapRightShape (Round l r) = Round l (findShape (shapeToOutcome r) l)

-- | Reinterpret the shapes as outcomes, for part 2.
shapeToOutcome :: Shape -> Outcome
shapeToOutcome Rock = Loss
shapeToOutcome Paper = Draw
shapeToOutcome Scissors = Win

-- | Find a shape that gets the desired outcome against another shape.
findShape :: Outcome -> Shape -> Shape
findShape Win Rock = Paper
findShape Win Paper = Scissors
findShape Win Scissors = Rock
findShape Loss Paper = Rock
findShape Loss Scissors = Paper
findShape Loss Rock = Scissors
findShape Draw s = s

parse :: String -> [Round]
parse = map pLine . lines
  where
    pLine [pLhs -> l, ' ', pRhs -> r] = Round l r
    pLine _ = error "Unexpected input format"

    pLhs 'A' = Rock
    pLhs 'B' = Paper
    pLhs 'C' = Scissors
    pLhs c = error $ "Unexpected LHS " ++ [c]

    pRhs 'X' = Rock
    pRhs 'Y' = Paper
    pRhs 'Z' = Scissors
    pRhs c = error $ "Unexpected RHS " ++ [c]
