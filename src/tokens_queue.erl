-module(tokens_queue).
-author("jianchuan.gys@taobao.com").

-behavior(gen_server).

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/0, start_link/1, set_max_cps/1, set_cps_interval/1, set_add_tokens_interval/1,
	request_tokens/0, i/0, infos/0]).

-define(default_max_cps, 100).
%% count_cps_interval must be bigger than add_tokens_interval
-define(count_cps_interval, 2000).
-define(add_tokens_interval, 100).

-define(state_tuple, {cps, cps_count, cps_start_time, count_cps_interval,
	total_tokens, count, max_cps, start_time, add_tokens_interval}).

-record(state, ?state_tuple).

start_link() ->
	start_link(?default_max_cps).

start_link(MAXCPS) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [MAXCPS], []).

set_max_cps(MAXCPS) ->
	gen_server:call(?MODULE, {set_max_cps, MAXCPS}).

set_cps_interval(CountCpsInterval) ->
	gen_server:call(?MODULE, {set_cps_interval, CountCpsInterval}).

set_add_tokens_interval(AddTokensInterval) ->
	gen_server:call(?MODULE, {set_add_tokens_interval, AddTokensInterval}).

i() ->
	gen_server:call(?MODULE, i).

infos() ->
	gen_server:call(?MODULE, infos).

request_tokens() ->
	gen_server:call(?MODULE, request_tokens).

init([MAXCPS]) ->
	{ok, #state{
		cps 					= 0,
		cps_count 				= 0,
		cps_start_time 			= get_now_time(),
		count_cps_interval 		= ?count_cps_interval,
		total_tokens 			= get_total_tokens(MAXCPS, ?add_tokens_interval),
		count 					= 0,
		max_cps					= MAXCPS,
		start_time 				= get_now_time(),
		add_tokens_interval 	= ?add_tokens_interval
	}, 0}.

handle_call(i, _From, State) ->
	io:format("~p~n", [?state_tuple]),
	{reply, State, State, get_time_left(State)};

handle_call(infos, _From, State) ->
	Infos = get_infos_from_state(State),
	{reply, Infos, State, get_time_left(State)};

handle_call({set_max_cps, MAXCPS}, _From, State = #state{add_tokens_interval = AddTokensInterval}) ->
	{reply, ok, State#state{max_cps = MAXCPS, 
	total_tokens = get_total_tokens(MAXCPS, AddTokensInterval)}, get_time_left(State)};

handle_call({set_cps_interval, CountCpsInterval}, _From, State) ->
	{reply, ok, State#state{count_cps_interval = CountCpsInterval}, get_time_left(State)};

handle_call({set_add_tokens_interval, AddTokensInterval}, _From, State = #state{max_cps = MaxCps}) ->
	{reply, ok, State#state{add_tokens_interval = AddTokensInterval, 
	total_tokens = get_total_tokens(MaxCps, AddTokensInterval)}, get_time_left(State)};

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

handle_cast(Event, State) ->
    {noreply, {unknown_cast, Event}, State}.

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

get_total_tokens(MaxCps, Interval) ->
	MaxCps * Interval div 1000.

add_tokens(State = #state{cps_count = CpsCount, cps_start_time = CpsStartTime, 
	count_cps_interval = CountCpsInterval}) ->

	% io:format("Infos ~p NowTime ~p~n", [get_infos_from_state(State), get_now_time()]),

	State0 = State#state{count = 0, start_time = get_now_time()},

	Interval = get_now_time() - CpsStartTime,

	State1 = case  Interval >= CountCpsInterval of
		true ->
			NowCps = CpsCount / Interval * 1000,
			io:format("Current cps is ~p~n", [NowCps]),
			State0#state{cps = NowCps, cps_count = 0, cps_start_time = get_now_time()};
		false ->
			State0
	end,
	State1.

get_infos_from_state(State) ->
	{_, Infos} = lists:foldl(
		fun(Info, {Count, Result}) ->
			{Count + 1, [{Info, element(Count + 1, State)} | Result]}
		end,
	{1, []}, tuple_to_list(?state_tuple)),
	Infos.
