-module(demo_worker).

-compile(export_all).

start(Pid) ->
	start(10000, Pid).

start(N, Pid) ->
	spawn(fun() ->
		for(N, fun() -> tokens_queue:request_tokens(Pid) end)
	end).

start_with_callback(Pid) ->
	start_with_callback(10000, Pid).

start_with_callback(N, Pid) ->
	spawn(fun() ->
		Self = self(),
		tokens_queue:set_callback_function(Pid, fun() -> Self ! wakeup end),
		for(N, fun() ->
			tokens_queue:request_tokens_cast(Pid),
			receive
				wakeup ->
					ok
			end
		end)
	end).

for (0, _Func) ->
	ok;
for (N, Func) ->
	Func(),
	for (N - 1, Func).
