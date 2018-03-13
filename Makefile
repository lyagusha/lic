ERL ?= erl
APP := ebm
REBAR ?= rebar --verbose 0

.PHONY: all test clean deps

all:
	@$(REBAR) skip_deps=true compile

deps:
	@$(REBAR) get-deps
	@$(REBAR) update-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit
