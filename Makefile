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

all: info clean test

info:
	$(info erl program located at $(ERL))
	$(info ERL_LIBS set to $(ERL_LIBS))

check:
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE check-deps)

compile: check
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE compile)
	@(escript -f hrlgen)

clean:
	@(./rebar clean)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: compile
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE test)
