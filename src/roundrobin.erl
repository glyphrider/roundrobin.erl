-module(roundrobin).

-export([start/0]).

start() ->
    lager:start(),
    application:start(roundrobin).
