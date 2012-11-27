-module(tokens_queue_sup).
-author("jianchuan.gys@taobao.com").

-behaviour(supervisor).

-export([init/1]).

-export([start_link/0, start_child/0, start_child/1, delete_child/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
	do_start_child([]).

start_child(MaxCps) ->
	do_start_child([MaxCps]).

delete_child(Pid) ->
	supervisor:terminate_child(?MODULE, Pid).

do_start_child(Args) ->
	supervisor:start_child(?MODULE, Args).

init([]) ->
	RestartStategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStategy, [
			{tokens_queue, {tokens_queue, start_link, []}, 
			temporary, brutal_kill, worker, [tokens_queue]}
		]}}.
