-module(sr_simulate).

-export([start/0, stop/0]).

start() ->
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).