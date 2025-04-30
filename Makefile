default: clean build

.PHONY: repl debug test clean build web

repl:
	rlwrap dune exec abstract_machines -- -machine $(MACHINE)

debug:
	rlwrap dune exec abstract_machines -- -machine $(MACHINE) -debug

build:
	dune build ./bin/main.exe

web:
	dune build ./bin/main.bc.js
	xdg-open ./bin/index.html

test: clean build
	dune test

clean:
	dune clean
