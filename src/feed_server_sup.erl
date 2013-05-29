-module(feed_server_sup).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-behaviour(supervisor).
-export([start_link/0,init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(feed_sup, supervisor),
                                  ?CHILD(feed_server, worker)]} }.
