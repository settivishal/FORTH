module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Fixed(mod')
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

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

-- Modulo
-- integer modulo operation
eval "%" (Integer x: Integer y:tl) = Integer (x `mod` y) : tl
-- real modulo operation
eval "%" (Real x: Real y:tl) = Real (x `mod'` y) : tl
-- integer base, real modulo
eval "%" (Integer x: Real y:tl) = Real (fromIntegral x `mod'` y) : tl
-- real base, integer modulo
eval "%" (Real x: Integer y:tl) = Real (x `mod'` fromIntegral y) : tl
-- error
eval "%" [] = error "Stack underflow"
eval "%" [_] = error "Stack underflow"
eval "%" _ = error "Type mismatch in %"

-- Negation
-- negation for integers
eval "NEG" (Integer x : tl) = Integer (-x) : tl
-- negation for real numbers
eval "NEG" (Real x : tl)    = Real (-x) : tl
-- error for empty stack
eval "NEG" []               = error "Stack underflow"
-- error for type mismatch
eval "NEG" _                = error "Type mismatch in NEG"

-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- EMIT
eval "EMIT" (Integer n : tl) 
    | n >= 0 && n <= 127 = Id [toEnum (fromIntegral n) :: Char] : tl
    | otherwise = error "Invalid ASCII code in EMIT"
-- error
eval "EMIT" _ = error "Type mismatch in EMIT"

-- STR: Converts the argument into a string
eval "STR" (x:tl) = case x of
    Integer i -> Id (show i) : tl
    Real r    -> Id (printf "%.2f" r) : tl  -- Use printf to avoid scientific notation
    Id s      -> Id s : tl
    -- _         -> error "Type mismatch in STR"
-- error
eval "STR" [] = error "Stack underflow"

-- CONCAT2: Concatenates two strings from the stack
eval "CONCAT2" [Id str1, Id str2] = [Id (str1 ++ str2)]
-- error
eval "CONCAT2" _ = error "Type mismatch in CONCAT2"

-- CONCAT3: Concatenates three strings from the stack
eval "CONCAT3" (Id str1: Id str2: Id str3: tl) = Id (str1 ++ str2 ++ str3) : tl
-- error
eval "CONCAT3" _ = error "Type mismatch in CONCAT3"

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
-- evalOut "." (Id x:tl, out) = (Id x:tl, out ++ "\"" ++ x ++ "\"")
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- CR: prints a new line for formatting
evalOut "CR" (stack, out) = (stack, out ++ "\n")

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)