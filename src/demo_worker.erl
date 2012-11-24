-module(demo_worker).

-compile(export_all).

start(TokensId) ->
	start(10000, TokensId).

start(N, TokensId) ->
	spawn(fun() ->
		for(N, fun() -> tokens_queue_manager:request_tokens(TokensId) end)
	end).

for (0, _Func) ->
	ok;
for (N, Func) ->
	Func(),
	for (N - 1, Func).
