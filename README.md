# FORTH Interpreter

A Haskell implementation of a FORTH interpreter.

## Overview

This project implements a FORTH interpreter in Haskell. The interpreter is capable of processing FORTH code files and executing the commands within them.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler)
- [Cabal](https://www.haskell.org/cabal/) (Common Architecture for Building Applications and Libraries)

**Install dependencies**:
   The project uses `hspec`, `quickcheck`, and `flow` libraries for testing and flow control. To install these, run the following commands:

   ```bash
   cabal install hspec --lib
   cabal install quickcheck --lib
   cabal install flow --lib
   ```

## Installation

Clone the repository and navigate to the project directory:

```bash
git clone <repository-url>
cd forth-interpreter
```

## Build & Run Instructions

The project uses a Makefile to simplify the build and run process.

### Building the Project

To build the project, run:

```bash
make build
```

This command runs `cabal build` to compile the Haskell code.

### Updating Dependencies

To update Cabal's package database:

```bash
make update
```

### Installing the Program

To install the program:

```bash
make install
```

This runs `cabal install` with the `--overwrite-policy=always` flag.

### Running the Interpreter

To run the FORTH interpreter with test files:

```bash
make run args=<test file path>
```

The `args` parameter specifies the FORTH files to be processed. You can provide one or multiple files.

Example of running all test files:

```bash
make run args="tests/t1.4TH tests/t2.4TH tests/t3.4TH tests/t4.4TH tests/t5.4TH tests/t6.4TH tests/t7.4TH tests/t8.4TH tests/t9.4TH tests/t10.4TH"
```

### Running Tests in EvalSpec.hs

To run the test suite:

```bash
make test
```

This executes the Haskell test file `EvalSpec.hs` using `runhaskell`.

### Cleaning the Project

To clean build artifacts:

```bash
make clean
```

## Project Structure

- `tests/` - Test FORTH programs (*.4TH files)
- `EvalSpec.hs` - Test specifications for evaluating the interpreter

## Implementation Notes

### FORTH Language Features Implemented

- Basic arithmetic operations (`+`, `-`, `*`, `/`, `^`, `%`, `MOD`, `NEG`)
- Stack operations (`DUP`, `EMIT`, `STR`, `CR`, `CONCAT2`, `CONCAT3`)

### Challenges Encountered

1. **Stack Management**: Implementing the stack operations required careful tracking of stack state during execution.

2. **Control Flow**: The non-traditional control flow in FORTH (with `IF`, `BEGIN`, etc.) needed special handling in the parser and evaluator.

3. **Word Definitions**: Creating a mechanism to define and later execute custom words (functions) was complex, requiring a dictionary structure to store these definitions.

4. **Variable Scope**: Managing variable scope and ensuring proper variable resolution during execution required careful design.

5. **Error Handling**: Robust error handling was implemented to catch common FORTH programming errors like stack underflow or undefined words.

## Testing Approach

- The test suite in `EvalSpec.hs` covers various aspects of the interpreter.

- The `tests/` directory contains FORTH programs to verify the interpreter's functionality.
