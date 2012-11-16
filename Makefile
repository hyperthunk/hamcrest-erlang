
.PHONY: clean build test

all: clean test

clean:
	@(./rebar clean)

build:
	@(./rebar compile xref)

test: build
	@(./rebar -C test.config get-deps compile)
	@(./rebar -C test.config skip_deps=true ct)
