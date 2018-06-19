all: watch

clean:
	@stack clean

prepare: clean
	@stack build --copy-compiler-tool hoogle brittany apply-refact weeder hspec hpack

install: clean
	@stack install

watch: clean prepare
	@stack test --fast --haddock-deps --file-watch
