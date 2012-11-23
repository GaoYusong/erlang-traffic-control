-module(tokens_queue_sup).
-author("jianchuan.gys@taobao.com").

-behaviour(supervisor).

-export([init/1]).

-export([start_link/0, start_child/1, start_child/2, delete_child/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(TokensQueueName) ->
	do_start_child(TokensQueueName, []).

start_child(TokensQueueName, MaxCps) ->
	do_start_child(TokensQueueName, [MaxCps]).

delete_child(TokensQueueName) ->
	case supervisor:terminate_child(?MODULE, TokensQueueName) of
		ok -> 
			supervisor:delete_child(?MODULE, TokensQueueName);
		{error, Reason} ->
			{error, Reason}
	end.

do_start_child(TokensQueueName, Args) ->
	Spec = {
		TokensQueueName, {tokens_queue, start_link, Args},
		permanent, 5000, worker, [tokens_queue]
	},
	supervisor:start_child(?MODULE, Spec).

init([]) ->
	RestartStategy = {one_for_one, 10, 10},
	{ok, {RestartStategy, []}}.
