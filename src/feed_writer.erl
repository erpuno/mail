-module(feed_writer).
-behaviour(feed_consumer).
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/meetings.hrl").
-include_lib("kvs/include/log.hrl").
-include_lib("feed_server/include/feed_server.hrl").
-export([init/1, handle_notice/3, get_opts/1, handle_info/2, handle_call/3, start_link/2,
        cached_feed/3, cached_direct/3, feed_refresh/3, direct_refresh/3,
        cached_friends/2, cached_groups/1 ]).

-record(state, {
        owner = "feed_owner",
        type :: user | group | system, 
        feed,
        direct,
        cached_feed,
        cached_direct,
        cached_friends,
        cached_groups }).

start_link(Mod,Args) -> gen_server:start_link(Mod, Args, []).

init(Params) -> 
    Owner   = proplists:get_value(name,   Params),
    Type    = proplists:get_value(type,   Params),
    Feed    = proplists:get_value(feed,   Params),
    Direct  = proplists:get_value(direct, Params, undefined),
    gproc:reg({p,l,Owner}, {Type,Feed,Direct}),
    ?INFO("Init worker: ~p", [Params]),
    {ok, #state{owner = Owner, type = Type, feed = Feed, direct = Direct}}.

cached_feed(Uid,Fid,Page) -> Pid = feed_server:pid(Uid), gen_server:call(Pid,{cached_feed,Fid,Page}).
cached_direct(Uid,Fid,Page) -> Pid = feed_server:pid(Uid), gen_server:call(Pid,{cached_direct,Fid,Page}).
cached_friends(Id,Type) -> Pid = feed_server:pid(Id), gen_server:call(Pid,{cached_friends,Id,Type}).
cached_groups(Id) -> Pid = feed_server:pid(Id), gen_server:call(Pid,{cached_groups,Id}).

feed_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{feed_refresh,Fid,Page}).
direct_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{direct_refresh,Fid,Page}).

handle_call({cached_feed,FId,Page},From,State) ->
    ?INFO("Old State: ~p",[State]),
    Reply = case State#state.cached_feed of
                 undefined -> kvs:entries_in_feed(FId,Page);
                 A -> A end,
    NewState = State#state{cached_feed=Reply},
    ?INFO("New State: ~p",[NewState]),
    {reply,Reply,NewState};

handle_call({cached_direct,FId,Page},From,State) ->
    Reply = case State#state.cached_direct of
                 undefined -> kvs:entries_in_feed(FId,Page);
                 A -> A end,
    {reply,Reply,State#state{cached_direct=Reply}};

handle_call({cached_friends,Id,Type},From,State) ->
    ?INFO("~p Old State: ~p",[self(), State]),
    Reply = case State#state.cached_friends of
                 undefined -> users:retrieve_connections(Id,Type);
                 A -> A end,
    NewState = State#state{cached_friends=Reply},
    ?INFO("New State: ~p",[NewState]),
    {reply,Reply,NewState};

handle_call({cached_groups,User},From,State) ->
    Reply = case State#state.cached_groups of
                 undefined -> groups:retrieve_groups(User);
                 A -> A end,
    {reply,Reply,State#state{cached_groups=Reply}};

handle_call({feed_refresh,FId,Page},From,State) -> Reply = kvs:entries_in_feed(FId,Page), {reply,ok,State#state{cached_feed=Reply}};
handle_call({direct_refresh,FId,Page},From,State) -> Reply = kvs:entries_in_feed(FId,Page), {reply,ok,State#state{cached_direct=Reply}}.
handle_info({feed_refresh, Fid,  Page}, State) -> Reply = kvs:entries_in_feed(Fid,Page), {noreply, State#state{cached_feed=Reply}};
handle_info({direct_refresh, Fid, Page}, State) -> Reply = kvs:entries_in_feed(Fid,Page), {noreply, State#state{cached_direct=Reply}};
handle_info(_Info, State) -> {noreply, State}.

handle_notice(Route, Message, State) ->
    feed_server_api:handle(Route, Message, State).

get_opts(#state{type = user, owner = Owner}) ->
    Name = ?FEED_WORKER_NAME(user, Owner),
    %% feeds workers queues has form: feed.worker.Owner
    QueueName = mqs_lib:list_to_key(Name),
    [{routes, [""]},
     {queue, QueueName},
     %% all binds have to be done for this exchange
     {exchange, ?USER_EXCHANGE(Owner)},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue_options, queue_options()}];

get_opts(#state{type = group, owner = Owner}) ->
    Name = ?FEED_WORKER_NAME(group, Owner),
    %% feeds workers queues has form: feed.worker.Owner
    QueueName = mqs_lib:list_to_key(Name),
    %% group worker listen only for direct messages
    %% and special message to stop worker
    [{routes, [""]},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue, QueueName},
     {exchange, ?GROUP_EXCHANGE(Owner)},
     {queue_options, queue_options()}];

get_opts(#state{type = system, owner = Owner}) ->
    Name = ?FEED_WORKER_NAME(group, Owner),
    QueueName = mqs_lib:list_to_key(Name),
    [{routes, [[system, put],
                [system, delete],
                [system, create_group],
                [system, add_package],
                [system, use_invite],
                [system, count_user],
                [system, meeting_join],
                [system, meeting_remove]
                ]},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue, QueueName},
     {queue_options, queue_options()}].

coalesce(undefined, B) -> B;
coalesce(A, _) -> A.

queue_options() ->
    [durable,
     {ttl, 10000},
     {auto_delete, false},
     {dead_letter_exchange, ?DEAD_LETTER_EXCHANGE}].

