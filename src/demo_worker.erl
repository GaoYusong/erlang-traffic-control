-module(demo_worker).

-compile(export_all).


start(Pid) ->
	start(1000000, Pid).

start(N, Pid) ->
	spawn(fun() ->
		for(N, fun() -> tokens_queue:request_tokens(Pid) end)
	end).

for (0, _Func) ->
	ok;
for (N, Func) ->
	Func(),
	for (N - 1, Func).

start_with_callback(Pid) ->
	spawn(fun() ->
		Self = self(),
		tokens_queue:set_callback_function(Pid, fun() -> Self ! wakeup end),
		tokens_queue:request_tokens_cast(Pid),
		loop(Pid)
	end).

loop(Pid) ->
	receive
		wakeup ->
			tokens_queue:request_tokens_cast(Pid),
			loop(Pid);
		_Other ->
			io:format("I am fine!~n"),
			loop(Pid)
	end.

