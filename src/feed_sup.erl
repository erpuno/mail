-module(feed_sup).
-behaviour(supervisor).
-export([init/1,start_worker/2,start_link/0]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_worker(CallbackModule, Params) ->
    Restart = transient,
    Shutdown = 200,
    Name = proplists:get_value(name,Params),
    Type = proplists:get_value(type,Params),
    ChildSpec = {{Type,Name}, {feed_consumer, start_link, [CallbackModule,Params]}, 
                              Restart, Shutdown, worker, [feed_consumer]},
    supervisor:start_child(?MODULE, ChildSpec).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
