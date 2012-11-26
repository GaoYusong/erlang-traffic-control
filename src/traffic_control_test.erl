-module(traffic_control_test).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(state_tuple, {t1, t2, t3}).
-record(state, ?state_tuple).

traffic_control_lib_test() ->

	State = #state{t1 = abc, t2 = 10, t3 = "abc"},

	Res = 
		traffic_control_lib:get_infos_from_state(
			State,
			?state_tuple
		),
	Ans = [{t3, "abc"}, {t2, 10}, {t1, abc}],
	?assertEqual(Ans, Res).

-endif.

