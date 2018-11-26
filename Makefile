ERL ?= erl
APP := lic
REBAR ?= rebar --verbose 0

all:
	@$(REBAR) compile

deps:
	@$(REBAR) get
	@$(REBAR) compile

clean:
	@$(REBAR) clean
