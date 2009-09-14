run:
	ghci -Wall Format

clean:
	rm -rf dist

docs:
	cabal configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Format/index.html

