-module(tokens_queue).
-author("jianchuan.gys@taobao.com").

-behavior(gen_server).

-include("tokens_queue.hrl").

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/0, start_link/1, set_max_cps/2, 
	% set_cps_interval/2, set_add_tokens_interval/2,
	request_tokens/1, i/1, infos/1]).

%% count_cps_interval must be bigger than add_tokens_interval
-define(count_cps_interval, 2000).
%% add_tokens_interval must can be diveded 1000 exactly.
-define(add_tokens_interval, 100).
%% this value equals 1000 div add_tokens_interval
-define(add_tokens_total, 10).

-define(state_tuple, {cps, cps_count, cps_start_time, count_cps_interval,
	total_tokens, total_tokens_base, total_tokens_reminder, total_tokens_r_count, 
	count, max_cps, start_time, add_tokens_interval, add_tokens_total}).

-record(state, ?state_tuple).

start_link() ->
	start_link(?default_max_cps).

start_link(MAXCPS) ->
    gen_server:start_link(?MODULE, [MAXCPS], []).

set_max_cps(Pid, MAXCPS) ->
	gen_server:call(Pid, {set_max_cps, MAXCPS}).

% set_cps_interval(Pid, CountCpsInterval) ->
% 	gen_server:call(Pid, {set_cps_interval, CountCpsInterval}).

% set_add_tokens_interval(Pid, AddTokensInterval) ->
% 	gen_server:call(Pid, {set_add_tokens_interval, AddTokensInterval}).

i(Pid) ->
	gen_server:call(Pid, i).

infos(Pid) ->
	gen_server:call(Pid, infos).

request_tokens(Pid) ->
	gen_server:call(Pid, request_tokens).

init([MAXCPS]) ->
	State0 = 
		#state{
			cps 					= 0,
			cps_count 				= 0,
			cps_start_time 			= get_now_time(),
			count_cps_interval 		= ?count_cps_interval,
			% total_tokens 			= get_total_tokens(MAXCPS, ?add_tokens_interval),
			% total_tokens_r_count 	= 0,
			% total_tokens_reminder	= 0,
			count 					= 0,
			max_cps					= MAXCPS,
			start_time 				= get_now_time(),
			add_tokens_interval 	= ?add_tokens_interval,
			add_tokens_total		= ?add_tokens_total
		},
	State1 = set_total_tokens(State0, MAXCPS, ?add_tokens_total),
	{ok, State1, 0}.

handle_call(i, _From, State) ->
	io:format("~p~n", [?state_tuple]),
	{reply, State, State, get_time_left(State)};

handle_call(infos, _From, State) ->
	Infos = sr_simulate_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State, get_time_left(State)};

handle_call({set_max_cps, MAXCPS}, _From, State = #state{add_tokens_total = AddTokensTotal}) ->
	State0 = State#state{max_cps = MAXCPS},
	State1 = set_total_tokens(State0, MAXCPS, AddTokensTotal),
	{reply, ok, State1, get_time_left(State)};

% handle_call({set_cps_interval, CountCpsInterval}, _From, State) ->
% 	{reply, ok, State#state{count_cps_interval = CountCpsInterval}, get_time_left(State)};

% handle_call({set_add_tokens_interval, AddTokensInterval}, _From, State = #state{max_cps = MaxCps}) ->
% 	{reply, ok, State#state{add_tokens_interval = AddTokensInterval, 
% 	total_tokens = get_total_tokens(MaxCps, AddTokensInterval)}, get_time_left(State)};

handle_call(request_tokens, _From, State = #state{cps_count = CpsCount, count = Count, total_tokens = TotalTokens}) ->
	case Count < TotalTokens of
		true ->
			{reply, ok, State#state{cps_count = CpsCount + 1, count = Count + 1}, get_time_left(State)};
		false ->
			timer:sleep(get_time_left(State)),
			State1 = add_tokens(State),
			{reply, ok, State1#state{cps_count = State1#state.cps_count + 1, count = State1#state.count + 1}, 
				State#state.add_tokens_interval}
	end.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
	State1 = add_tokens(State),
    {noreply, State1, State#state.add_tokens_interval}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_time_left(State) ->
	time_left(State#state.start_time, State#state.add_tokens_interval).

%% get now time in ms
get_now_time() ->
	{A, B, C} = now(),
	((A * 1000000 + B) * 1000000 + C) div 1000.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, Interval) ->
    case Interval - (get_now_time() - StartTime) of
        Time when Time =< 0 -> 0;
        Time -> Time
    end.

% get_total_tokens(MaxCps, Interval) ->
% 	MaxCps * Interval div 1000.

add_tokens(State 
	% = #state{cps_count = CpsCount, cps_start_time = CpsStartTime, count_cps_interval = CountCpsInterval}
	) ->

	% io:format("Infos ~p NowTime ~p~n", [sr_simulate_lib:get_infos_from_state(State, ?state_tuple), get_now_time()]),

	NowTime = get_now_time(),

	State0 = State#state{
		count = 0, start_time = NowTime
	},

	% Interval = NowTime - CpsStartTime,
	% State1 = case  Interval >= CountCpsInterval of
	% 	true ->
	% 		NowCps = CpsCount / Interval * 1000,
	% 		% io:format("Current cps is ~p~n", [NowCps]),
	% 		State0#state{cps = NowCps, cps_count = 0, cps_start_time = NowTime};
	% 	false ->
	% 		State0
	% end,
	State1 = State0,

	State2 = calc_total_tokens(State1),
	State2.

set_total_tokens(State, MaxCps, AddTokensTotal) ->
	TotalTokens = MaxCps div AddTokensTotal,
	Reminder 	= MaxCps rem AddTokensTotal, 
	State0 = State#state{
		total_tokens_base		= TotalTokens,
		total_tokens_reminder 	= Reminder,
		total_tokens_r_count 	= 0
	},
	State0.

calc_total_tokens(State = #state{
		total_tokens_base		= TotalTokensBase,
		total_tokens_reminder 	= Reminder,
		total_tokens_r_count 	= RCount,
		add_tokens_total		= AddTokensTotal
	}) ->
	RCount0 = 
		case RCount < AddTokensTotal of
			true ->
				RCount;
			false ->
				0
		end,
	TotalTokens = 
		case RCount0 < Reminder of
			true ->
				TotalTokensBase + 1;
			false ->
				TotalTokensBase
		end,
	State#state{total_tokens = TotalTokens, total_tokens_r_count = RCount0 + 1}.
