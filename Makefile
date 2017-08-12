all: watch

clean:
	@stack clean

install: clean
	@stack install

watch: clean
	@stack build --test --fast --file-watch --exec '$(MAKE) plot'

plot: .
	@find samples/ -type f -name '*.png' -delete
	@find samples/ -type f -name '*.dot' -delete
	@find samples/ -type f | xargs -n 1 -P 4 -I % bash -c 'dot -Tpng <(stack exec copts graph "`cat %`") -o %.png'
