all: watch

clean:
	@stack clean

install: clean
	@stack install

watch: clean
	@stack test --fast --haddock-deps --file-watch
