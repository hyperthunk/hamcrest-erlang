# -----------------------------------------------------------------------------
#
# Hamcrest Erlang.
#
# Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# -----------------------------------------------------------------------------

ERL ?= `which erl`
ERL_LIBS := $(shell echo "./deps:`echo $$ERL_LIBS`")
VERBOSE ?= ""
HERE := $(shell pwd)
BUILD := $(HERE)/build
ERTS_VSN := $(shell escript scripts/checkvsn "5.7.5")
ifeq ($(ERTS_VSN), 0)
    INCL_TYPES = "-b"
endif
INCL_TYPES ?= "NOTYPES=1"
all: info clean test

info:
	$(info erl program located at $(ERL))
	$(info ERL_LIBS set to $(ERL_LIBS))

deps: build
	mkdir -p $@

build:
	mkdir -p $@

deps/proper: deps
	@(echo "y" | env ERL_LIBS=$(ERL_LIBS) HOME=$(BUILD) ./epm install manopapad/proper \
                --build-command "make $(INCL_TYPES)" \
                --config-set build_dir $(BUILD) \
                --config-set install_dir $(HERE)/deps $$VERBOSE)

check: deps/proper
	@(env ERL_LIBS=$(ERL_LIBS) ./rebar $$VERBOSE get-deps check-deps)

compile: check
	@(env ERL_LIBS=$(ERL_LIBS) ./rebar $$VERBOSE compile)

clean:
	@(echo "y" | env HOME=$(BUILD) ./epm remove proper $(VERBOSE))
	@(./rebar $$VERBOSE clean delete-deps)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: compile
	@(env ERL_LIBS=$(ERL_LIBS) ./rebar $$VERBOSE ct skip_deps=true)
