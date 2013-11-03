-module(feed_server_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_, _) -> case feed_server_sup:start_link() of {ok, Pid} -> {ok, Pid}; E -> E end.
stop(_State) -> ok.
