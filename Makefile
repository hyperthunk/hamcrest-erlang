
ERL ?= `which erl`
VERBOSE ?= ""
HERE := $(shell pwd)

all: clean build test

clean:
	@(./rebar clean)

build:
	./rebar get-deps compile

test:
	./rebar skip_deps=true ct -v

.PHONY: deps test clean build
