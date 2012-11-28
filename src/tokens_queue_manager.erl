-module(tokens_queue_manager).
-author("jianchuan.gys@taobao.com").

-behaviour(gen_server).

-include("tokens_queue.hrl").

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, infos/0, 
	new_tokens_queue/0, new_tokens_queue/1, delete_tokens_queue/1
	% , request_tokens/1
	]).

-define(state_tuple, {}).


-record(state, ?state_tuple).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

infos() ->
	gen_server:call(?MODULE, infos).

new_tokens_queue() ->
	new_tokens_queue(?default_max_cps).

new_tokens_queue(MaxCps) ->
	% gen_server:call(?MODULE, {new_tokens_queue, MaxCps}).
	tokens_queue_sup:start_child(MaxCps).

delete_tokens_queue(Pid) ->
	% gen_server:call(?MODULE, {delete_tokens_queue, Pid}).
	tokens_queue_sup:delete_child(Pid).


% request_tokens(Name) ->
% 	gen_server:call(?MODULE, {request_tokens, Name}).

init([]) ->
	{ok, #state{}}.

handle_call(infos, _From, State) ->
	Infos = traffic_control_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call({new_tokens_queue, MaxCps}, _From, State) ->
	Result = tokens_queue_sup:start_child(MaxCps),
	{reply, Result, State};

handle_call({delete_tokens_queue, Pid}, _From, State) ->
	Result = tokens_queue_sup:delete_child(Pid),
	{reply, Result, State}.

handle_cast(_Event, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
