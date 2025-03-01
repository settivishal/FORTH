module Main where

import Interpret
import System.Environment
import System.Environment (getArgs)
import Control.Monad (forM_)

-- Function to check and print stack status
checkStack :: Show a => [a] -> IO ()
checkStack [] = putStrLn "The stack is empty."
checkStack stack = do
    putStrLn "The stack is not empty. Current stack content:"
    print stack
    putStrLn ""

main :: IO ()
main = do
    -- Get the command-line arguments (file path)
    args <- getArgs
    forM_ args $ \file -> do
        putStrLn $ "Processing file: " ++ file
        -- Read the file content
        contents <- readFile file
        -- Interpret the contents
        let (stack, output) = interpret contents
        
        -- Print the output from the interpretation
        putStrLn output
        
        -- Check and print the status of the stack
        checkStack stack
