# See LICENSE for licensing information.

DIALYZER = dialyzer
PLT = .ex_http_dialyzer.plt
REBAR = rebar

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt $(PLT) \
		--apps kernel stdlib \
		-pa deps/*/ebin deps/*/ebin

dialyze:
	@$(DIALYZER) --src src --plt $(PLT) -pa deps/*/ebin \
		-Wbehaviours -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
