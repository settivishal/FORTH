module Eval where
-- This file contains definitions for functions and operators

import Val

roundTo5 :: Float -> Float
roundTo5 x = fromIntegral (round (x * 100000)) / 100000

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]

-- Addition
-- integers
eval "+" (Integer x: Integer y:tl) = Integer (x+y) : tl
-- real numbers
eval "+" (x:y:tl) = Real (toFloat x + toFloat y) : tl
-- error
eval "+" _ = error "Stack underflow"

-- Subtraction
-- integers
eval "-" (Integer x: Integer y:tl) = Integer (x-y) : tl
-- real numbers
eval "-" (x:y:tl) = Real (toFloat x - toFloat y) : tl
-- errors
eval "-" _ = error "Stack underflow"

-- Multiplication
-- integers
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- real numbers
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error "Stack underflow"

-- Division
-- integer division
eval "/" (Integer x: Integer y:tl)
  | y == 0    = error "Division by zero not allowed"
  | otherwise = Real (roundTo5(fromIntegral x / fromIntegral y)) : tl
-- floating-point division
eval "/" (Real x: Real y:tl)
  | y == 0    = error "Division by zero not allowed"
  | otherwise = Real (roundTo5(x / y)) : tl  
-- floating-point division(convert x integer to float)
eval "/" (Integer x: Real y:tl)
  | y == 0    = error "Division by zero not allowed"
  | otherwise = Real (roundTo5(fromIntegral x / y)) : tl 
  -- floating-point division(convert y integer to float)
eval "/" (Real x: Integer y:tl)
  | y == 0    = error "Division by zero not allowed"
  | otherwise = Real (roundTo5(x / fromIntegral y)) : tl 
-- error
eval "/" _ = error "Stack underflow"

-- Exponentiation
-- integer exponentiation
eval "^" (Integer x: Integer y:tl) = Integer (x ^ y) : tl
-- real exponentiation
eval "^" (Real x: Real y:tl) = Real (x ** y) : tl
-- integer base, real exponent
eval "^" (Integer x: Real y:tl) = Real (fromIntegral x ** y) : tl
-- real base, integer exponent
eval "^" (Real x: Integer y:tl) = Real (x ** fromIntegral y) : tl
-- error
eval "^" _ = error "Stack underflow"

-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)

-- Emit function
emit :: Stack -> IO Stack
emit (x:xs) = do
    putChar (toEnum x)
    return xs
emit _ = error "EMIT requires a non-empty stack"