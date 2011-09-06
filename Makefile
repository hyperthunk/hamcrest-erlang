
all: clean build test

clean:
	@(./rebar clean)

build:
	@(./rebar get-deps compile)

test:
	@(./rebar skip_deps=true ct)

.PHONY: deps test clean build
