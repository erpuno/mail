-module(writer).
-behaviour(gen_server).
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include("records.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {
        owner = "feed_owner",
        type :: user | group | system | product,
        feeds = [],
        callback,
        cached_feeds=[]}).

start_link(Params) -> gen_server:start_link(?MODULE, [Params], []).

init([Params]) ->
    error_logger:info_msg("init worker ~p", [Params]),

    gproc:reg({p,l,?MAIN_CH}), % broadcast bus

    {ok, #state{
        owner = proplists:get_value(id, Params),
        type  = proplists:get_value(type, Params),
        feeds = proplists:get_value(feeds, Params) }}.

handle_call(_,_,S) -> {reply,ok, S}.
handle_cast(_,S) -> {noreply, S}.
handle_info({delivery, [Module|_]=Route, Msg}, S) ->
    M = if is_atom(Module)-> Module;
        true-> list_to_atom(binary_to_list(term_to_binary(Module))) end,
    M:handle_notice(Route, Msg, S);
handle_info(_,S) -> {noreply, S}.
terminate(_,_) -> ok.
code_change(_,S,_) -> {ok, S}.
