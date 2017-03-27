{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid
import Text.Read (readMaybe)
import System.Random

data Config =
  Config
    { maxNumber :: Int
    , requiredCorrectGuesses :: Int
    , maxIncorrectGuesses :: Int
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { maxNumber = 2
    , requiredCorrectGuesses = 3
    , maxIncorrectGuesses = 6
    }

data Guess
  = Correct Int
  | Incorrect Int Int
  deriving (Eq, Show)

data Results =
  Results
    { guesses :: [Guess]
    , badInputs :: Int
    , correctChecksCount :: Int
    , incorrectChecksCount :: Int
    } deriving (Eq, Show)

initialResults :: Results
initialResults =
  Results
    { guesses = []
    , badInputs = 0
    , correctChecksCount = 0
    , incorrectChecksCount = 0
    }

main :: IO ()
main = do
  (outcome, results) <- run defaultConfig initialResults
  case outcome of
    Win -> putStrLn "You won!"
    Loss -> putStrLn "You lost!"
  print results

data Outcome = Win | Loss

run :: Config -> Results -> IO (Outcome, Results)
run cfg res = do
  print res
  answer <- randomRIO (0, maxNumber cfg)
  putStrLn $ "Guess a number inclusively between 0 and " <> show (maxNumber cfg)
  guess :: Maybe Int <- readMaybe <$> getLine
  res' <- case guess of
    Nothing -> do
      putStrLn $ "Couldn't parse your guess as a number"
      pure res { badInputs = badInputs res + 1 }
    Just num
      | answer == num ->
        if requiredCorrectGuesses cfg == correctGuesses res
          then pure res
          else do
            putStrLn $ "Congratulations! The answer was " <> show answer
            pure res { guesses = Correct answer : guesses res }
      | otherwise -> do
        putStrLn $ "Boo! You guessed " <> show num <> ", but the answer was: " <> show answer
        pure res { guesses = Incorrect answer num : guesses res }
  (enoughCorrect, res'') <- hasEnoughCorrectGuesses cfg res'
  if enoughCorrect
  then pure (Win, res'')
  else do
    (tooManyIncorrect, res''') <- hasTooManyIncorrectGuesses cfg res''
    if tooManyIncorrect
    then pure (Loss, res''')
    else run cfg res'''

hasEnoughCorrectGuesses :: Config -> Results -> IO (Bool, Results)
hasEnoughCorrectGuesses cfg res = do
  putStrLn "Checking for enough correct guesses"
  let required = requiredCorrectGuesses cfg
  let correct = correctGuesses res
  pure $ ( correct == required
         , res { correctChecksCount = correctChecksCount res + 1 }
         )

hasTooManyIncorrectGuesses :: Config -> Results -> IO (Bool, Results)
hasTooManyIncorrectGuesses cfg res = do
  putStrLn "Checking for too many incorrect guesses"
  let maxIncorrect = maxIncorrectGuesses cfg
  let incorrect = incorrectGuesses res
  pure $ ( incorrect >= maxIncorrect
         , res { incorrectChecksCount = incorrectChecksCount res + 1 }
         )

correctGuesses :: Results -> Int
correctGuesses = length . filter isCorrect . guesses
  where
    isCorrect :: Guess -> Bool
    isCorrect Incorrect {} = False
    isCorrect Correct {} = True

incorrectGuesses :: Results -> Int
incorrectGuesses = length . filter isCorrect . guesses
  where
    isCorrect :: Guess -> Bool
    isCorrect Incorrect {} = True
    isCorrect Correct {} = False
