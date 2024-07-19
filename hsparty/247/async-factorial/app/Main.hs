module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data CalculationRequest = CalculationRequest Integer deriving Show
data CalculationResult = CalculationResult Integer Integer deriving Show

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

asyncFactorial :: CalculationRequest -> IO CalculationResult
asyncFactorial (CalculationRequest n) = do
    let result = factorial n
    return $ CalculationResult n result

handleInput :: TQueue String -> IO ()
handleInput inputQueue = forever $ do
    putStr "Enter a non-negative integer (or 'q' to quit): "
    hFlush stdout
    input <- getLine
    atomically $ writeTQueue inputQueue input

handleCalculation :: TQueue String -> TQueue (Maybe CalculationResult) -> IO ()
handleCalculation inputQueue outputQueue = forever $ do
    input <- atomically $ readTQueue inputQueue
    case input of
        "q" -> atomically $ writeTQueue outputQueue Nothing
        _ -> case readMaybe input of
            Just n | n >= 0 -> do
                result <- asyncFactorial (CalculationRequest n)
                atomically $ writeTQueue outputQueue (Just result)
            _ -> putStrLn "Invalid input. Please enter a non-negative integer."

handleOutput :: TQueue (Maybe CalculationResult) -> IO ()
handleOutput outputQueue = go
  where
    go = do
        maybeResult <- atomically $ readTQueue outputQueue
        case maybeResult of
            Just (CalculationResult n result) -> do
                putStrLn $ "The factorial of " ++ show n ++ " is " ++ show_with_commas result
                go
            Nothing -> return ()

main :: IO ()
main = do
    putStrLn "Welcome to the Asynchronous Reactive Factorial Calculator!"
    
    inputQueue <- newTQueueIO
    outputQueue <- newTQueueIO
    
    void $ forkIO $ handleInput inputQueue
    void $ forkIO $ handleCalculation inputQueue outputQueue
    handleOutput outputQueue
    
    putStrLn "Thank you for using the Asynchronous Reactive Factorial Calculator. Goodbye!"

-- Helper function to format large numbers with commas; Works in a weird way that is yet to be fixed
show_with_commas :: Integer -> String
show_with_commas = reverse . foldr insert "" . reverse . show
  where
    insert d s
      | length s `mod` 3 == 0 && not (null s) = d : ',' : s
      | otherwise = d : s
