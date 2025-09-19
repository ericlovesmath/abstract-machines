default: clean build

.PHONY: repl debug test clean build web

repl:
	rlwrap dune exec abstract_machines -- -machine $(MACHINE)

debug:
	rlwrap dune exec abstract_machines -- -machine $(MACHINE) -debug

visualize:
	# May take a while, you can just view with xdot too
	dot -Tsvg -O ./logs/*.dot
	feh --scale-down ./logs/*.svg

web:
	dune build --profile release ./bin/web.bc.js
	xdg-open ./bin/index.html

test: clean build
	dune test

clean:
	rm -f ./logs/*.dot
	rm -f ./logs/*.dot.svg
	dune clean

build:
	dune build
