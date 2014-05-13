
.PHONY: app dialyzer eunit

app : src/*.erl deps
	rebar compile

deps :
	rebar get

dialyzer : roundrobin.plt

roundrobin.plt : ebin/*.beam
	[ -f $@ ] || dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib crypto mnesia -r deps/goldrush/ebin deps/rebar/ebin
	dialyzer --plt $@ $<

eunit : src/*.erl
	rebar eunit skip_deps=true
