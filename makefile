all: build update install

build:
	cabal build

update:
	cabal update

install:
	cabal install --overwrite-policy=always

# make run args="tests/t1.4TH tests/t2.4TH tests/t3.4TH tests/t4.4TH tests/t5.4TH tests/t6.4TH tests/t7.4TH tests/t8.4TH tests/t9.4TH tests/t10.4TH"
run:
	cabal run FORTH $(args)

test:
	runhaskell EvalSpec.hs

clean:
	cabal clean
