-module(workers_sup).
-behaviour(supervisor).
-export([init/1, start_child/1, start_link/0]).
-define(CHILD(Id, I, Type, P), {Id, {I, start_link, [P]}, transient, 200, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child(Params) ->
    Type  = proplists:get_value(type, Params),
    Id    = proplists:get_value(id, Params),
    supervisor:start_child(?MODULE, ?CHILD({Type,Id}, writer, worker, Params)).
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
