Erlang traffic control
=============================

[![Build Status](https://secure.travis-ci.org/GaoYusong/erlang-traffic-control.png?branch=master)](https://travis-ci.org/GaoYusong/erlang-traffic-control)

A traffic control application written in erlang, you can use it to control the rate of flow, now it is implemented by using token buckets algorithm.

## Two ways to use it
It can be used by block or non-block your application, and you can find demo in src/demo_worker.erl.
### Block demo
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
        
### Run block demo
    {ok, Pid} = tokens_queue_manager:new_tokens_queue(100),
    demo_worker:start(Pid).
    
    %% show status
    tokens_queue:infos(Pid).

### Non-block demo
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
### Run non-block demo
    {ok, Pid} = tokens_queue_manager:new_tokens_queue(100),
    UserPid = demo_worker:start_with_callback(Pid).
    
    %% show status
    tokens_queue:infos(Pid).


## Unified MySQL Platform 

This application is used in proxy module in [Unified MySQL Platform](http://blog.yufeng.info/archives/2349) from taobao to control sql query per second.

## Important

If you want to use this application, please using branch stable.

## TODO

* Add a good tutorial.
* Add a lisence file, maybe BSD like license is good choice.

