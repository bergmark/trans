{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid
import Text.Read (readMaybe)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
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
  undefined <- flip runStateT initialResults $ flip runReaderT defaultConfig run
--  case undefined of
--    Win -> putStrLn "You won!"
--    Loss -> putStrLn "You lost!"
--  print results
  pure ()

data Outcome = Win | Loss

type Game = ReaderT Config (StateT Results IO)

run :: (MonadReader r m, MonadState Results m, MonadIO m) => m (Outcome, Results)
run = do
  cfg <- ask
  res <- lift $ get
  answer :: Int <- liftIO $ randomRIO (0, maxNumber cfg)
  liftIO $ putStrLn $ "Guess a number inclusively between 0 and " <> show (maxNumber cfg)
  guess :: Maybe Int <- readMaybe <$> liftIO getLine
  case guess of
    Nothing -> do
      liftIO $ putStrLn $ "Couldn't parse your guess as a number"
      modify (\res -> res { badInputs = badInputs res + 1 })
    Just num
      | answer == num ->
        if requiredCorrectGuesses cfg == correctGuesses res
          then pure res
          else do
            liftIO $ putStrLn $ "Congratulations! The answer was " <> show answer
            modify $ \res -> res { guesses = Correct answer : guesses res }
      | otherwise -> do
        liftIO $ putStrLn $ "Boo! You guessed " <> show num <> ", but the answer was: " <> show answer
        modify $ \res -> res { guesses = Incorrect answer num : guesses res }
  enoughCorrect <- hasEnoughCorrectGuesses res'
  if enoughCorrect
  then pure Win
  else do
    tooManyIncorrect <- hasTooManyIncorrectGuesses
    if tooManyIncorrect
    then pure Loss
    else run

hasEnoughCorrectGuesses :: (MonadReader r m, MonadIO m) => m (Outcome, Results)
hasEnoughCorrectGuesses = do
  cfg <- ask
  liftIO $ putStrLn "Checking for enough correct guesses"
  let required = requiredCorrectGuesses cfg
  res <- get
  let correct = correctGuesses res
  modify $ \res -> res { correctChecksCount = correctChecksCount res + 1 }
  pure $ correct == required

hasTooManyIncorrectGuesses :: Game Bool
hasTooManyIncorrectGuesses = do
  cfg <- ask
  liftIO $ putStrLn "Checking for too many incorrect guesses"
  let maxIncorrect = maxIncorrectGuesses cfg
  res <- get
  let incorrect = incorrectGuesses res
  modify $ \res -> res { incorrectChecksCount = incorrectChecksCount res + 1 }
  pure $ incorrect >= maxIncorrect

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
