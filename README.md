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

Extract the archive and follow the commands in the next section.

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

## FORTH Interpreter Project Structure

```
FORTH/
├── dist-newstyle/            # Cabal build directory
├── tests/                    # Test FORTH programs
│   ├── t1.4TH                # Test file 1
│   ├── t2.4TH                # Test file 2
│   ├── t3.4TH                # Test file 3
│   ├── t4.4TH                # Test file 4
│   ├── t5.4TH                # Test file 5
│   ├── t6.4TH                # Test file 6
│   ├── t7.4TH                # Test file 7
│   ├── t8.4TH                # Test file 8
│   ├── t9.4TH                # Test file 9
│   └── t10.4TH               # Test file 10
├── Eval.hs                   # Evaluation logic for FORTH code
├── EvalSpec.hs               # Test specifications for Eval module
├── FORTH.cabal               # Cabal project file
├── Interpret.hs              # Core interpreter implementation
├── InterpretSpec.hs          # Test specifications for Interpret module
├── Main.hs                   # Main program entry point
├── makefile                  # Makefile with build commands
├── README.md                 # Project documentation
├── Requirements.md           # Project requirements documentation
├── Setup.hs                  # Cabal setup script
├── Val.hs                    # Value representation for FORTH
└── ValSpec.hs                # Test specifications for Val module
```

### Core Components

- **Main.hs**: Entry point of the application, handles command-line arguments and orchestrates the interpretation process.
- **Eval.hs**: Contains the evaluation logic for FORTH expressions and statements.

### Testing

- **EvalSpec.hs**: Unit tests for the evaluation logic
- **tests/*.4TH**: FORTH code files used for testing the interpreter

### Build System

- **FORTH.cabal**: Cabal configuration file defining project metadata, dependencies, and build instructions
- **makefile**: Contains build, test, and run commands for the project

### Documentation

- **README.md**: Overview, installation, and usage instructions
- **Requirements.md**: Detailed requirements and specifications for the project


## Implementation Notes

### FORTH Language Features Implemented

- Basic arithmetic operations (`+`, `-`, `*`, `/`, `^`, `%`, `MOD`, `NEG`)
- Stack operations (`DUP`, `EMIT`, `STR`, `CR`, `CONCAT2`, `CONCAT3`)

## Testing Approach

- The test suite in `EvalSpec.hs` covers various aspects of the interpreter.

- The `tests/` directory contains FORTH programs to verify the interpreter's functionality.

### Challenges Encountered

1. **Stack Management**: Implementing the stack operations required careful tracking of stack state during execution.

3. **Word Definitions**: Creating a mechanism to define and later execute custom words (functions) was complex, requiring a dictionary structure to store these definitions.

5. **Error Handling**: Robust error handling was implemented to catch common FORTH programming errors like stack underflow or undefined words.
