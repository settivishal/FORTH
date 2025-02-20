module Main where

import Interpret
import System.Environment

-- Function to check and print stack status
checkStack :: Show a => [a] -> IO ()
checkStack [] = putStrLn "The stack is empty."
checkStack stack = do
    putStrLn "The stack is not empty. Current stack content:"
    print stack

main :: IO ()
main = do
    -- Get the command-line arguments (file path)
    (fileName:tl) <- getArgs
    -- Read the file content
    contents <- readFile fileName
    -- Interpret the contents (assumed to return a tuple with stack and output)
    let (stack, output) = interpret contents
    
    -- Print the output from the interpretation
    putStrLn output
    
    -- Check and print the status of the stack
    checkStack stack
