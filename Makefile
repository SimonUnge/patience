REBAR=`which rebar || ./rebar`

all: compile

compile:
	@$(REBAR) compile

start:
	erl -pa ebin deps/deckerl/ebin -eval 'application:ensure_all_started(patience_game)'

tests: eunit commontest

eunit:
	@$(REBAR) skip_deps=true eunit

commontest:
	@$(REBAR) ct

dialyze:
	@dialyzer --src src/*.erl test/*.erl

clean:
	@$(REBAR) clean
	@rm -r ebin/ || true
	@rm -r logs/ || true
	@rm ct/*.beam || true
