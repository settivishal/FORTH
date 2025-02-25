import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

isCloseEnough :: Float -> Float -> Bool
isCloseEnough x y = abs (x - y) < 0.0001

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "+" $ do
        it "adds integers" $ do
            -- 1
            eval "+" [Integer 5, Integer 10] `shouldBe` [Integer 15]
            -- 2
            eval "+" [Integer 6, Integer 90] `shouldBe` [Integer 96]
        it "adds foating numbers" $ do
            -- 1
            eval "+" [Real 2.5, Real 10.5] `shouldBe` [Real 13.0]
            -- 2
            eval "+" [Integer 5, Real 1.3] `shouldBe` [Real 6.3]
            -- 3
            eval "+" [Real 5.7, Integer 6] `shouldBe` [Real 11.7]
            -- 4
            eval "+" [Real 9.9, Real 5.1] `shouldBe` [Real 15.0]
        it "too few arguments" $ do
            -- 1
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"
            -- 2
            evaluate (eval "+" [Integer 30]) `shouldThrow` errorCall "Stack underflow"

    context "-" $ do
        it "subtracts integers" $ do
            -- 1
            eval "-" [Integer 5, Integer 10] `shouldBe` [Integer (-5)]
            -- 2
            eval "-" [Integer 20, Integer 4] `shouldBe` [Integer 16]
        it "subtracts foating numbers" $ do
            -- 1
            let result = eval "-" [Real 2.5, Real 10.5]
            case result of
                [Real x] -> x `shouldSatisfy` isCloseEnough (-8.0)
                _        -> expectationFailure "Expected a single Real value"
            -- 2
            let result = eval "-" [Integer 5, Real 1.3]
            case result of
                [Real x] -> x `shouldSatisfy` isCloseEnough 3.7
                _        -> expectationFailure "Expected a single Real value"
            -- 3
            let result = eval "-" [Real 5.5, Integer 6]
            case result of
                [Real x] -> x `shouldSatisfy` isCloseEnough (-0.5)
                _        -> expectationFailure "Expected a single Real value"
            -- 4
            let result = eval "-" [Real 9.9, Real 5.1]
            case result of
                [Real x] -> x `shouldSatisfy` isCloseEnough 4.8
                _        -> expectationFailure "Expected a single Real value"
        it "too few arguments" $ do
            -- 1
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"
            -- 2
            evaluate (eval "-" [Integer 5]) `shouldThrow` errorCall "Stack underflow"

    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "/" $ do
        it "divide integers" $ do
            -- 1
            eval "/" [Integer 10, Integer 5] `shouldBe` [Real 2.0]
            -- 2
            eval "/" [Integer 6, Integer 100] `shouldBe` [Real 0.06]
            -- 3
            eval "/" [Integer 59, Integer 3] `shouldBe` [Real 19.66667]
        it "divides floating numbers" $ do
            -- 1
            eval "/" [Real 10.0, Real 5.0] `shouldBe` [Real 2.0]
            -- 2
            eval "/" [Integer 6, Real 100.0] `shouldBe` [Real 0.06]
            -- 3
            eval "/" [Real 40.0, Integer 2] `shouldBe` [Real 20.0]
            -- 4
            eval "/" [Real 22.0, Integer 4] `shouldBe` [Real 5.5]
        it "too few arguments" $ do
            -- 1
            evaluate (eval "/" []) `shouldThrow` errorCall "Stack underflow"
            -- 2
            evaluate (eval "/" [Integer 30]) `shouldThrow` errorCall "Stack underflow"
        it "divide by zero" $ do
            -- 1
            evaluate (eval "/" [Integer 6, Integer 0]) `shouldThrow` errorCall "Division by zero not allowed"

    context "^" $ do
        it "raises integers to a power" $ do
            -- 1
            eval "^" [Integer 2, Integer 3] `shouldBe` [Integer 8]
            -- 2
            eval "^" [Integer 5, Integer 0] `shouldBe` [Integer 1]
            -- 3
            eval "^" [Integer 7, Integer 1] `shouldBe` [Integer 7]

        it "raises real numbers to a power" $ do
            -- 1
            eval "^" [Real 2.0, Real 3.0] `shouldBe` [Real 8.0]
            -- 2
            eval "^" [Real 4.0, Real 0.5] `shouldBe` [Real 2.0]
            -- 3
            eval "^" [Real 9.0, Real 0.5] `shouldBe` [Real 3.0]

        it "handles integer base with real exponent" $ do
            -- 1
            eval "^" [Integer 2, Real 3.5] `shouldBe` [Real 11.313708]
            -- 2
            eval "^" [Integer 10, Real 2.0] `shouldBe` [Real 100.0]

        it "handles real base with integer exponent" $ do
            -- 1
            eval "^" [Real 2.5, Integer 2] `shouldBe` [Real 6.25]
            -- 2
            eval "^" [Real 3.0, Integer 3] `shouldBe` [Real 27.0]

        it "errors on too few arguments" $ do
            -- 1
            evaluate (eval "^" []) `shouldThrow` errorCall "Stack underflow"
            -- 2
            evaluate (eval "^" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    -- Modulo
    context "%" $ do
        it "modulus integers" $ do
            -- 1
            eval "%" [Integer 11, Integer 5] `shouldBe` [Integer 1]
            -- 2
            eval "%" [Integer 6, Integer 100] `shouldBe` [Integer 6]
            -- 3
            eval "%" [Integer 59, Integer 3] `shouldBe` [Integer 2]
        it "modulus floating numbers" $ do
            -- 1
            eval "%" [Real 10.0, Real 5.0] `shouldBe` [Real 0.0]
            -- 2
            eval "%" [Integer 6, Real 100.0] `shouldBe` [Real 6.0]
            -- 3
            eval "%" [Real 40.0, Integer 2] `shouldBe` [Real 0.0]
            -- 4
            eval "%" [Real 22.0, Integer 4] `shouldBe` [Real 2.0]
        it "too few arguments" $ do
            -- 1
            evaluate (eval "%" []) `shouldThrow` errorCall "Stack underflow"
            -- 2
            evaluate (eval "%" [Integer 30]) `shouldThrow` errorCall "Stack underflow"

    -- Negation
    context "NEG" $ do
        it "negates integers" $ do
            -- 1
            eval "NEG" [Integer 5] `shouldBe` [Integer (-5)]
            -- 2
            eval "NEG" [Integer (-3)] `shouldBe` [Integer 3]
        it "negates floating numbers" $ do
            -- 1
            eval "NEG" [Real 2.5] `shouldBe` [Real (-2.5)]
            -- 2
            eval "NEG" [Real (-7.1)] `shouldBe` [Real 7.1]
        it "errors on empty stack" $ do
            -- 1
            evaluate (eval "NEG" []) `shouldThrow` errorCall "Stack underflow"
        it "errors on type mismatch" $ do
            -- 1
            evaluate (eval "NEG" []) `shouldThrow` errorCall "Stack underflow"

    -- Duplicate
    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"

    -- EMIT
    context "EMIT" $ do
        it "prints the character corresponding to ASCII code" $ do
            -- For ASCII 65 (A)
            eval "EMIT" [Integer 65] `shouldBe` []  -- The character "A" will be printed to the console

            -- For ASCII 97 (a)
            eval "EMIT" [Integer 97] `shouldBe` []  -- The character "a" will be printed to the console

            -- For ASCII 48 (0)
            eval "EMIT" [Integer 48] `shouldBe` []  -- The character "0" will be printed to the console

        it "throws error on non-integer value" $ do
            evaluate (eval "EMIT" [Real 65.0]) `shouldThrow` errorCall "Type mismatch in EMIT"
            evaluate (eval "EMIT" [Id "not an integer"]) `shouldThrow` errorCall "Type mismatch in EMIT"

    -- STR
    context "STR" $ do
        it "converts integers to strings" $ do
            eval "STR" [Integer 5] `shouldBe` [Id "5"]
            eval "STR" [Integer (-123)] `shouldBe` [Id "-123"]

        it "converts real numbers to strings" $ do
            eval "STR" [Real 3.14] `shouldBe` [Id "3.14"]
            eval "STR" [Real (-0.01)] `shouldBe` [Id "-0.01"]

        it "converts Ids to strings (no change)" $ do
            eval "STR" [Id "hello"] `shouldBe` [Id "hello"]
            eval "STR" [Id "42"] `shouldBe` [Id "42"]

        it "throws error on empty stack" $ do
            evaluate (eval "STR" []) `shouldThrow` errorCall "Stack underflow"

        -- CONCAT2
    context "CONCAT2" $ do
        it "concatenates two strings" $ do
            eval "CONCAT2" [Id "Hello", Id " World"] `shouldBe` [Id "Hello World"]
            eval "CONCAT2" [Id "foo", Id "bar"] `shouldBe` [Id "foobar"]
            eval "CONCAT2" [Id "123", Id "abc"] `shouldBe` [Id "123abc"]
        
        it "throws error if there are not exactly two arguments" $ do
            evaluate (eval "CONCAT2" [Id "Hello"]) `shouldThrow` errorCall "Type mismatch in CONCAT2"
            evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Type mismatch in CONCAT2"

    -- CONCAT3
    context "CONCAT3" $ do
        it "concatenates three strings" $ do
            eval "CONCAT3" [Id "Hello", Id " ", Id "World"] `shouldBe` [Id "Hello World"]
            eval "CONCAT3" [Id "foo", Id "bar", Id "baz"] `shouldBe` [Id "foobarbaz"]
            eval "CONCAT3" [Id "1", Id "2", Id "3"] `shouldBe` [Id "123"]
        
        it "throws error if there are not exactly three arguments" $ do
            evaluate (eval "CONCAT3" [Id "Hello", Id "World"]) `shouldThrow` errorCall "Type mismatch in CONCAT3"
            evaluate (eval "CONCAT3" [Id "foo", Id "bar"]) `shouldThrow` errorCall "Type mismatch in CONCAT3"
            evaluate (eval "CONCAT3" []) `shouldThrow` errorCall "Type mismatch in CONCAT3"

  describe "evalOut" $ do
    context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` ([Id "x"],"x")
            evalOut "." ([Integer 2], "") `shouldBe` ([Integer 2], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` ([Real 2.2], "2.2")

        it "errors on empty stack" $ do
            evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

        it "eval pass-through" $ do
            evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah")

    -- CR (Newline)
    context "CR" $ do
        it "prints a newline" $ do
            evalOut "CR" ([Id "x"], "") `shouldBe` ([Id "x"], "\n")
            evalOut "CR" ([Integer 2], "") `shouldBe` ([Integer 2], "\n")
            evalOut "CR" ([Real 2.2], "") `shouldBe` ([Real 2.2], "\n")

        it "does not alter the stack" $ do
            evalOut "CR" ([Id "x", Integer 3], "") `shouldBe` ([Id "x", Integer 3], "\n")
            evalOut "CR" ([Real 3.0, Integer 5], "") `shouldBe` ([Real 3.0, Integer 5], "\n")

        it "works when stack is empty" $ do
            evalOut "CR" ([], "") `shouldBe` ([], "\n")

        it "preserves previous output" $ do
            evalOut "CR" ([Id "x"], "First output") `shouldBe` ([Id "x"], "First output\n")
            evalOut "CR" ([Real 3.0], "Initial text") `shouldBe` ([Real 3.0], "Initial text\n")