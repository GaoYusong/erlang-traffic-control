-module(traffic_control_lib).

-export([get_infos_from_state/2]).


get_infos_from_state(State, StateTuple) ->
	{_, Infos} = lists:foldl(
		fun(Info, {Count, Result}) ->
			{Count + 1, [{Info, element(Count + 1, State)} | Result]}
		end,
	{1, []}, tuple_to_list(StateTuple)),
	Infos.
