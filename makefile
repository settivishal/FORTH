all: build update install

build:
	cabal build

update:
	cabal update

install:
	cabal install --overwrite-policy=always

run:
	cabal run FORTH $(args)

test:
	runhaskell EvalSpec.hs

clean:
	cabal clean
