-module(tokens_queue).
-author("jianchuan.gys@taobao.com").

-behavior(gen_server).

-include("tokens_queue.hrl").

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/0, start_link/1, 
	set_max_cps/2, set_max_cps_cast/2,
	% set_cps_interval/2, set_add_tokens_interval/2,
	request_tokens/1, i/1, infos/1]).

%% count_cps_interval must be bigger than add_tokens_interval
-define(count_cps_interval, 2000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% begin add_tokens configure 

%% important:
%% please read the comment and double check before you 
%% change these values.

%% add_tokens_interval must can be diveded 1000 exactly.
%% important: 
%% if you change this value, please change add_tokens_total too,
%% and the add_tokens_total = 1000 / add_tokens_interval
-define(add_tokens_interval, 100).

%% this value equals 1000 div add_tokens_interval
%% important:
%% if you change this value, please change add_tokens_interval too,
%% and the add_tokens_interval = 1000 / add_tokens_total
-define(add_tokens_total, 10).

%% same to before, this will be used in max_cps >= 5 && max_cps < 10 
-define(add_tokens_interval_0, 200).
-define(add_tokens_total_0, 5).

%% same to before, this will be used in max_cps >= 1 && max_cps < 5
-define(add_tokens_interval_1, 1000).
-define(add_tokens_total_1, 1).

%% end add_tokens configure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(state_tuple, {
	%% for cps statistics
	cps, cps_count, cps_start_time, count_cps_interval,
	%% for tokens limits
	count, total_tokens,  
	%% for max_cps set
	max_cps, total_tokens_base, total_tokens_reminder, total_tokens_r_count,
	set_add_tokens_interval, set_add_tokens_total,
	%% current add_tokens interval control
	start_time, add_tokens_interval, add_tokens_total}).

-record(state, ?state_tuple).

start_link() ->
	start_link(?default_max_cps).

start_link(MAXCPS) ->
    gen_server:start_link(?MODULE, [MAXCPS], []).

set_max_cps(Pid, MAXCPS) ->
	gen_server:call(Pid, {set_max_cps, MAXCPS}).

set_max_cps_cast(Pid, MAXCPS) ->
	gen_server:cast(Pid, {set_max_cps_cast, MAXCPS}).

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

%% important: double check every state has a initial value
init([MAXCPS]) ->
	State0 = 
		#state{
			%% for cps
			cps 					= 0,
			cps_count 				= 0,
			cps_start_time			= get_now_time(),
			count_cps_interval		= ?count_cps_interval

			%% for count, these will be set in add_tokens
			% count 					= 0,
			% total_tokens 			= get_total_tokens(MAXCPS, ?add_tokens_interval),

			%% for add_tokens, these will be set in add_tokens
			% start_time 				= get_now_time(),
			% add_tokens_interval 	= ?add_tokens_interval,
			% add_tokens_total		= ?add_tokens_total
		},
	State1 = set_total_tokens(State0, MAXCPS),
	State2 = add_tokens(State1),
	{ok, State2, State2#state.add_tokens_interval}.

handle_call(i, _From, State) ->
	io:format("~p~n", [?state_tuple]),
	{reply, State, State, get_time_left(State)};

handle_call(infos, _From, State) ->
	Infos = traffic_control_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State, get_time_left(State)};

handle_call({set_max_cps, MAXCPS}, _From, State) ->
	State0 = set_total_tokens(State, MAXCPS),
	{reply, ok, State0, get_time_left(State)};

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

handle_cast({set_max_cps_cast, MAXCPS}, State) ->
	State0 = set_total_tokens(State, MAXCPS),
	{noreply, State0, get_time_left(State)};

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
		= #state{
			cps_count = CpsCount, cps_start_time = CpsStartTime, count_cps_interval = CountCpsInterval,
			set_add_tokens_interval = SetAddTokensInterval, set_add_tokens_total = SetAddTokensTotal
		}
	) ->

	% io:format("Infos ~p NowTime ~p~n", [traffic_control_lib:get_infos_from_state(State, ?state_tuple), get_now_time()]),

	NowTime = get_now_time(),

	State0 = State#state{
		count = 0, 
		start_time = NowTime, add_tokens_interval = SetAddTokensInterval, add_tokens_total = SetAddTokensTotal
	},

	Interval = NowTime - CpsStartTime,
	State1 = case  Interval >= CountCpsInterval of
		true ->
			NowCps = CpsCount / Interval * 1000,
			% io:format("Current cps is ~p~n", [NowCps]),
			State0#state{cps = NowCps, cps_count = 0, cps_start_time = NowTime};
		false ->
			State0
	end,
	% State1 = State0,

	%% add_tokens_total must be set before call calc_total_tokens
	State2 = calc_total_tokens(State1),
	State2.

set_total_tokens(State, MaxCps) ->
	{SetAddTokensInterval, SetAddTokensTotal}
		= get_set_add_tokens_info(MaxCps),

	TotalTokens = MaxCps div SetAddTokensTotal,
	Reminder 	= MaxCps rem SetAddTokensTotal, 
	State0 = State#state{
		max_cps 				= MaxCps,
		total_tokens_base		= TotalTokens,
		total_tokens_reminder 	= Reminder,
		total_tokens_r_count 	= 0,
		set_add_tokens_interval = SetAddTokensInterval,
		set_add_tokens_total 	= SetAddTokensTotal
	},
	State0.

get_set_add_tokens_info(MaxCps) ->
	case MaxCps >= ?add_tokens_total of
		true ->
			{?add_tokens_interval, ?add_tokens_total};
		false ->
			case MaxCps >= ?add_tokens_total_0 of
				true ->
					{?add_tokens_interval_0, ?add_tokens_total_0};
				false ->
					{?add_tokens_interval_1, ?add_tokens_total_1}
			end
	end.

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

