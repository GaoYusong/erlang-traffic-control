# Erlang Traffic Control

A traffic control library written in Erlang allows you to control the rate of flow using the token bucket algorithm.

## Notice

1. The project has been stable for two years, running reliably in our system.
2. I wrote this project when I was an Erlang learner, so the code style may not be optimal...

## Two Ways to Use It
The library can be used in either blocking or non-blocking mode. You can find a demonstration in src/demo_worker.erl.

### Blocking Demo
```erlang
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
```

### Running the Blocking Demo
```erlang
{ok, Pid} = tokens_queue_manager:new_tokens_queue(100),
demo_worker:start(Pid).

%% show status
tokens_queue:infos(Pid).
```

### Non-Blocking Demo
```erlang
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
```
### Running the Non-Blocking Demo
```erlang
{ok, Pid} = tokens_queue_manager:new_tokens_queue(100),
UserPid = demo_worker:start_with_callback(Pid).

%% show status
tokens_queue:infos(Pid).
```


## Unified MySQL Platform 

This library is used in the proxy module of [Unified MySQL Platform](http://blog.yufeng.info/archives/2349) by Taobao to control SQL queries per second.

## TODO

* Add a comprehensive tutorial.
* Add a license file, preferably a BSD-like license.

