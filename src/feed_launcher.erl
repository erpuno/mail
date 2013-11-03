-module(feed_launcher).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-behaviour(feed_consumer).
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include("feed_server.hrl").
-export([init/1, handle_notice/3, handle_info/2, get_opts/1, start_worker/4, start_link/2]).
-record(state, {name,type}).

start_link(Mod,Args) -> gen_server:start_link(Mod, Args, []).

init(Params) -> 
    Name = proplists:get_value(name,Params),
    Type = proplists:get_value(type,Params),
    {ok, #state{name = Name,type=Type}}.

handle_notice(["user", "init"], Message, State) ->
    start_worker(user, Message, undefined, undefined),
    {noreply, State};

handle_notice(Route, Message, State) ->
    error_logger:info_msg("Launcher notification received: ", [self(), Route, Message]),
    {noreply, State}.

start_worker(Name, Type, Feed, Direct) ->
    feed_sup:start_worker(feed_writer, [{name,Name},{feed,Feed},{type,Type},{direct,Direct}]).

handle_info(_Other, State) -> {noreply, State}.

get_opts(_State) ->
    [{routes, [ [user, init], [group, init], [system, init] ]},
     {grpoc_name, [bootstrap, worker, node(), utils:uuid_ex()]},
     {queue, ?BOOTSTRAP_WORKER_QUEUE},
     {queue_options, [{auto_delete, false}, durable]}].
