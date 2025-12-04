# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o boxes Main.hs

prof:
	ghc --make -prof -o boxes Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f boxes
	rm -f *.hi
	rm -f *.o

setup:
	cabal install ansi-terminal
