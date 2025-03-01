all: build update install

build:
	@cabal build

update:
	@cabal update

install:
	@cabal install --overwrite-policy=always

run:
	@cabal run FORTH $(args)

TEST_FILES=$(wildcard tests/*.4TH)

run-all-tests:
	@make run args="$(TEST_FILES)"

test:
	@runhaskell EvalSpec.hs

clean:
	@cabal clean
