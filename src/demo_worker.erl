-module(demo_worker).

-compile(export_all).

start() ->
	tokens_queue:start_link(),
	spawn(fun() ->
		for(10000, fun() -> tokens_queue:request_tokens(), timer:sleep(1) end)
	end).

for (0, _Func) ->
	ok;
for (N, Func) ->
	Func(),
	for (N - 1, Func).
