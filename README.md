Erlang traffic control
=============================

[![Build Status](https://secure.travis-ci.org/GaoYusong/erlang-traffic-control.png?branch=master)](https://travis-ci.org/GaoYusong/erlang-traffic-control)

A traffic control application written in erlang, you can use it to control the rate of flow, now it is implemented by using token buckets algorithm.

## Two ways to use it
It can be used by block or non-block your application, and you can find demo in src/demo_worker.erl.
### Block
    {ok, Pid} = tokens_queue_manager:new_tokens_queue(100),
    demo_worker:start(Pid).
    
    %% show status
    tokens_queue:infos(Pid).

### Non-blocking
    {ok, Pid} = tokens_queue_manager:new_tokens_queue(100),
    UserPid = demo_worker:start_with_callback(Pid).
    
    %% show status
    tokens_queue:infos(Pid).


## Unified MySQL Platform 

This application is used in proxy module in [Unified MySQL Platform](http://blog.yufeng.info/archives/2349) from taobao to control sql query per second.

## Important

If you want to use this application, please using branch stable.

