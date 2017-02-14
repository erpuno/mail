-module(feed_server).
-behaviour(gen_server).
-include_lib("kvs/include/product.hrl").
-include_lib("kvs/include/group.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("kvs/include/entry.hrl").
-include_lib("kvs/include/comment.hrl").
-include_lib("kvs/include/user.hrl").
-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-record(state,{}).

start_link() -> case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
                    {ok,Pid} -> Pid ! start_all, {ok,Pid};
                    E -> error_logger:info_msg("[feed_server] Start failed! ~p", [E]) end.

init([])->
    gproc:reg({p,l,?MAIN_CH}),
    kvs:wait(),
    {ok, #state{}}.

handle_call(_,_,S) -> {reply, ok, S}.
handle_cast(_,S) -> {noreply, S}.
handle_info({delivery, Route, Msg}, S) -> handle_notice(Route, Msg), {noreply, S};
handle_info(start_all, S) ->
    error_logger:info_msg("Starting workers..."),

    [handle_notice([ok, user,    init], [Id, Feeds]) || #user{email=Id, feeds=Feeds} <- kvs:all(user)],
    [handle_notice([ok, group,   init], [Id, Feeds]) || #group{id = Id, feeds=Feeds} <- kvs:all(group)],
    % product table need to be descibed
    %[handle_notice([ok, product, init], [Id, Feeds]) || #product{id=Id, feeds=Feeds} <- kvs:all(product)],

    {noreply, S};
handle_info(_,S) -> {noreply, S}.
terminate(_,_S) -> ok.
code_change(_,S,_) -> {ok, S}.

handle_notice([kvs_user, user, create], [#user{}=U])->
    Created = case kvs:add(U) of {error,E} -> {error,E};
    {ok, #user{email=Id, feeds=Feeds}=User} ->
        Params = [{id, Id}, {type, user}, {feeds, Feeds}],
        case workers_sup:start_child(Params) of {error,E} -> {error,E}; _ -> User end end,

    msg:notify([kvs_user, user, created], [Created]);

% system MQ

handle_notice([_, Type, init], [Id, Feeds]) -> workers_sup:start_child([{id, Id}, {type, Type}, {feeds, Feeds}]);

handle_notice([_,_,_,entry,Eid,add], [#entry{feed_id=entries}=Entry]) -> kvs_feed:add_entry(Eid, entries, Entry);
handle_notice([_,_,_,entry,Eid,edit],[#entry{feed_id=entries}=Entry]) -> kvs_feed:update_entry(Eid, entries, Entry);
handle_notice([_,_,_,comment,_,add], [#comment{id={_,{_,entries},_}}=C]) -> kvs_feed:add_comment(C);
handle_notice(_,_) -> skip.
