ERL ?= `which erl`
ERL_LIBS := $(shell echo "./deps:`echo $$ERL_LIBS`")
VERBOSE ?= ""

all: info clean test

info:
	$(info erl program located at $(ERL))
	$(info ERL_LIBS set to $(ERL_LIBS))

compile:
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE compile)
	@(escript -f hrlgen)

clean:
	@(./rebar clean)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: compile
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE test)
