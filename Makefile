run:
	ghci -Wall Data.HoleyMonoid

clean:
	rm -rf dist

docs:
	cabal configure
	cabal haddock

opendocs: docs
	open dist/doc/html/HoleyMonoid/index.html

