-module(tokens_queue_manager).
-author("jianchuan.gys@taobao.com").

-behaviour(gen_server).

-include("tokens_queue.hrl").

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, infos/0, new_tokens_queue/0, new_tokens_queue/1,
	get_pid/1, delete_tokens_queue/1, request_tokens/1]).

-define(state_tuple, {tq_pids, name_count}).


-record(state, ?state_tuple).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

infos() ->
	gen_server:call(?MODULE, infos).

new_tokens_queue() ->
	new_tokens_queue(?default_max_cps).

new_tokens_queue(MaxCps) ->
	gen_server:call(?MODULE, {new_tokens_queue, MaxCps}).

delete_tokens_queue(Name) ->
	gen_server:call(?MODULE, {delete_tokens_queue, Name}).

get_pid(Name) ->
	gen_server:call(?MODULE, {get_pid, Name}).

request_tokens(Name) ->
	gen_server:call(?MODULE, {request_tokens, Name}).

init([]) ->
	{ok, #state{
		tq_pids 	= ets:new(tq_pids, [named_table]),
		name_count	= 0
	}}.

handle_call(infos, _From, State) ->
	Infos = traffic_control_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call({new_tokens_queue, MaxCps}, _From, State = #state{tq_pids = Pids, name_count = NameCount}) ->
	Result = 
		case tokens_queue_sup:start_child(NameCount, MaxCps) of
			{ok, Pid} -> 
				ets:insert(Pids, {NameCount, Pid}),
				{ok, NameCount};
			{error, Reason} ->
				{error, Reason}
		end,
	{reply, Result, State#state{name_count = NameCount + 1}};

handle_call({get_pid, Name}, _From, State = #state{tq_pids = Pids}) ->
	Result = do_get_pid(Pids, Name),
	{reply, Result, State};

handle_call({delete_tokens_queue, Name}, _From, State = #state{tq_pids = Pids}) ->
	Result = 
		case tokens_queue_sup:delete_child(Name) of
			ok ->
				ets:delete(Pids, Name),
				ok;
			{error, Reason} ->
				{error, Reason}
		end,
	{reply, Result, State};

handle_call({request_tokens, Name}, _From, State = #state{tq_pids = Pids}) ->
	Result = 
		case do_get_pid(Pids, Name) of
			{ok, Pid} ->
				tokens_queue:request_tokens(Pid),
				ok;
			{error, Reason} ->
				{error, Reason}
		end,
	{reply, Result, State}.

handle_cast(_Event, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

do_get_pid(Pids, Name) ->
	case ets:lookup(Pids, Name) of
		[] ->
			{error, tokens_queue_not_existed};
		[{_TQ, Pid}] ->
			{ok, Pid}
	end.
