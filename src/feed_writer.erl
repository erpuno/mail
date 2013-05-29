-module(feed_writer).
-behaviour(feed_consumer).
-include_lib("store/include/feed.hrl").
-include_lib("store/include/user.hrl").
-include_lib("store/include/meetings.hrl").
-include("feed_server.hrl").
-export([init/1, handle_notice/3, get_opts/1, handle_info/2, handle_call/3, start_link/2,
        cached_feed/3, cached_direct/3, feed_refresh/3, direct_refresh/3,
        cached_friends/2, cached_groups/1 ]).

-record(state, {owner = "feed_owner", type :: user | group | system, 
                feed, direct, cached_feed,cached_direct,cached_friends,cached_groups }).

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
%friends_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{friens_refresh,Fid,Page}).
%groups_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{groups_refresh,Fid,Page}).

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

handle_call({feed_refresh,FId,Page},From,State) ->
    Reply = kvs:entries_in_feed(FId,Page),
    {reply,ok,State#state{cached_feed=Reply}};

handle_call({direct_refresh,FId,Page},From,State) ->
    Reply = kvs:entries_in_feed(FId,Page),
    {reply,ok,State#state{cached_direct=Reply}}.

handle_notice(["feed", "delete", Owner] = Route, Message,
              #state{owner = Owner} = State) ->
    ?INFO("feed(~p): notification received: User=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    {stop, normal, State};

handle_notice(["feed", "group", GroupId, "entry", EntryId, "add"] = Route,
              [From|_] = Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): group message: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,
    feed:add_group_entry(Feed, From, [{GroupId, group}], EntryId,
                         Desc, Medias, {group, direct}),
    % statistics
    case Owner == GroupId of
        false -> ok;
        true ->
            {ok, Group} = kvs:get(group, GroupId),
            GE = Group#group.entries_count,
            kvs:put(Group#group{entries_count = GE+1}),
            {ok, Subs} = kvs:get(group_subs, {From, GroupId}),
            SE = Subs#group_subs.user_posts_count,
            kvs:put(Subs#group_subs{user_posts_count = SE+1})
    end,    

    self() ! {feed_refresh,Feed,20},
    {noreply, State};

handle_notice(["feed", "user", FeedOwner, "entry", EntryId, "add"] = Route,
              [From|_] = Message,
              #state{owner = WorkerOwner, feed = Feed, direct = Direct} = State) ->
    ?INFO("feed(~p): message: Owner=~p, Route=~p, Message=~p",
          [self(), WorkerOwner, Route, Message]),
    [From, Destinations, Desc, Medias] = Message,

    if
        %% user added message to own feed
        FeedOwner == From andalso FeedOwner == WorkerOwner->
            FilteredDst = [D || {_, group} = D <- Destinations],
            feed:add_entry(Feed, From, FilteredDst, EntryId, Desc, Medias,
                           {user, normal}), self() ! {feed_refresh,Feed,20};

        %% friend added message to public feed
        FeedOwner == From ->
            feed:add_entry(Feed, From, [], EntryId, Desc, Medias,
                           {user, normal}), self() ! {feed_refresh,Feed,20};

        %% direct message to worker owner
        FeedOwner == WorkerOwner ->
            feed:add_direct_message(Direct, From, [{FeedOwner, user}],
                                    EntryId, Desc, Medias), self() ! {direct_refresh,Direct,20};

        %% user sent direct message to friend, add copy to his direct feed
        From == WorkerOwner ->
            feed:add_direct_message(Direct, WorkerOwner, Destinations,
                                    EntryId, Desc, Medias), self() ! {direct_refresh,Direct,20};
        true ->
            ?INFO("not matched case in entry->add")
    end,
    
    {noreply, State};

% add/delete system message
handle_notice(["feed", "user", _FeedOwner, "entry", EntryId, "add_system"] = Route, 
              [From|_] = Message,
              #state{owner = WorkerOwner, feed = Feed, direct = _Direct} = State) ->
    ?INFO("feed(~p): system message: Owner=~p, Route=~p, Message=~p",
          [self(), WorkerOwner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,

    feed:add_entry(Feed, From, [], EntryId, Desc, Medias, {user, system}),
    {noreply, State};

handle_notice(["feed", "group", GroupId, "entry", EntryId, "add_system"] = Route,
              [From|_] = Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): group system message: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,
    feed:add_group_entry(Feed, From, [{GroupId, group}], EntryId,
                         Desc, Medias, {group, system}),
    {noreply, State};

handle_notice(["feed", "user", UId, "post_note"] = Route, Message, 
        #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): post_note: Owner=~p, Route=~p, Message=~p", [self(), Owner, Route, Message]),
    Note = Message,
    Id = utils:uuid_ex(),
    feed:add_entry(Feed, UId, [], Id, Note, [], {user, system_note}),
    {noreply, State};

handle_notice(["feed", _, WhoShares, "entry", NewEntryId, "share"],
              #entry{entry_id = _EntryId, raw_description = Desc, media = Medias,
                     to = Destinations, from = From} = E,
              #state{feed = Feed, type = user} = State) ->
    %% FIXME: sharing is like posting to the wall
    ?INFO("share: ~p, WhoShares: ~p", [E, WhoShares]),
%    NewEntryId = utils:uuid_ex(),
    feed:add_shared_entry(Feed, From, Destinations, NewEntryId, Desc, Medias, {user, normal}, WhoShares),
    {noreply, State};

handle_notice(["feed", "group", _Group, "entry", EntryId, "delete"] = Route,
              Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    %% all group subscribers shold delete entry from their feeds
    feed:remove_entry(Feed, EntryId),
    self() ! {feed_refresh,Feed,20},
    {noreply, State};

handle_notice(["feed", _Type, EntryOwner, "entry", EntryId, "delete"] = Route,
              Message,
              #state{owner = Owner} = State) ->
    case {EntryOwner, Message} of
        %% owner of the antry has deleted entry, we will delete it too
        {_, [EntryOwner|_]} ->
            ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
                  [self(), Owner, Route, Message]),
            remove_entry(State, EntryId);
        %% we are owner of the entry - delete it
        {Owner, _} ->
            ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
                  [self(), Owner, Route, Message]),
            remove_entry(State, EntryId);
        %% one of the friends has deleted some entry from his feed. Ignore
        _ ->
            ok
    end,
    self() ! {feed_refresh, State#state.feed,20},
    {noreply, State};

handle_notice(["feed", _Type, _EntryOwner, "entry", EntryId, "edit"] = Route,
              Message,
              #state{owner = Owner} = State) ->
    [NewDescription|_] = Message,
    ?INFO("feed(~p): edit: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),

    %% edit entry in all feeds
    edit_entry(State, EntryId, NewDescription),

    {noreply, State};

handle_notice(["feed", _Type, _EntryOwner, "comment", CommentId, "add"] = Route,
              Message,
              #state{owner = Owner} = State) ->
    [From, EntryId, ParentComment, Content, Medias] = Message,

    ?INFO("feed(~p): add comment: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    add_comment(State, From, EntryId, ParentComment,
                CommentId, Content, Medias),
    {noreply, State};

handle_notice(["feed", "user", UId, "count_entry_in_statistics"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): count_entry_in_statistics: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    case kvs:get(user_etries_count, UId) of
        {ok, UEC} -> 
            kvs:put(UEC#user_etries_count{
                entries = UEC#user_etries_count.entries+1
            }),
            users:attempt_active_user_top(UId, UEC#user_etries_count.entries+1);
        {error, notfound} ->
            kvs:put(#user_etries_count{
                user_id = UId,
                entries = 1
            }),
            users:attempt_active_user_top(UId, 1)
    end,
    {noreply, State};

handle_notice(["feed", "user", UId, "count_comment_in_statistics"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): count_comment_in_statistics: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    case kvs:get(user_etries_count, UId) of
        {ok, UEC} -> 
            kvs:put(UEC#user_etries_count{
                comments = UEC#user_etries_count.comments+1
            });
        {error, notfound} ->
            kvs:put(#user_etries_count{
                user_id = UId,
                comments = 1
            })
    end,
    {noreply, State};

handle_notice(["db", "group", Owner, "put"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): group put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    kvs:put(Message),
    {noreply, State};

handle_notice(["db", "user", Owner, "put"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): user put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    kvs:put(Message),
    {noreply, State};

handle_notice(["system", "put"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): system put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    kvs:put(Message),
    {noreply, State};

handle_notice(["system", "delete"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): system delete: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Where, What} = Message,
    kvs:delete(Where, What),
    {noreply, State};


handle_notice(["system", "create_group"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, GId, Name, Desc, Publicity} = Message,
    FId = kvs:feed_create(),
    CTime = erlang:now(),
    ok = kvs:put(#group{username = GId,
                              name = Name,
                              description = Desc,
                              publicity = Publicity,
                              creator = UId,
                              created = CTime,
                              owner = UId,
                              feed = FId}),
    nsx_msg:notify([group, init], {GId, FId}),
    users:init_mq_for_group(GId),
    add_to_group(UId, GId, member),
    {noreply, State};

handle_notice(["db", "group", GroupId, "update_group"] = Route, 
    Message, #state{owner=ThisGroupOwner, type=Type} = State) ->
    ?INFO("queue_action(~p): update_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, ThisGroupOwner}, Route, Message]),    
    {_UId, _GroupUsername, Name, Description, Owner, Publicity} = Message,
    SanePublicity = case Publicity of
        "public" -> public;
        "moderated" -> moderated;
        "private" -> private;
        _ -> undefined
    end,
    SaneOwner = case kvs:get(user, Owner) of
        {ok, _} -> Owner;
        _ -> undefined
    end,
    {ok, #group{}=Group} = kvs:get(group, GroupId),
    NewGroup = Group#group{
                   name = coalesce(Name,Group#group.name),
                   description = coalesce(Description,Group#group.description),
                   publicity = coalesce(SanePublicity,Group#group.publicity),
                   owner = coalesce(SaneOwner,Group#group.owner)},
    kvs:put(NewGroup),
    {noreply, State};

handle_notice(["db", "group", GId, "remove_group"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {_, Group} = groups:get_group(GId),
    case Group of 
        notfound -> ok;
        _ ->
            nsx_msg:notify([feed, delete, GId], empty),
            kvs:delete_by_index(group_subs, <<"group_subs_group_id_bin">>, GId),         
            kvs:delete(feed, Group#group.feed),
            kvs:delete(group, GId),
            % unbind exchange
            {ok, Channel} = mqs:open([]),
            Routes = users:rk_group_feed(GId),
            users:unbind_group_exchange(Channel, GId, Routes),
            mqs_channel:close(Channel)
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "add_to_group"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_to_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, Who, UType} = Message,
    add_to_group(Who, GId, UType),
    ?INFO("add ~p to group ~p with Type ~p by ~p", [Who, GId,UType,UId]),
    users:subscribe_user_mq(group, Who, GId),
    {noreply, State};

handle_notice(["subscription", "user", UId, "remove_from_group"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_from_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    {GId} = Message,
    ?INFO("remove ~p from group ~p", [UId, GId]),
    users:remove_subscription_mq(group, UId, GId),
    remove_from_group(UId, GId),
    {noreply, State};

handle_notice(["subscription", "user", UId, "leave_group"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): leave_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    {R, Group} = kvs:get(group, GId),
    case R of 
        error -> ?ERROR(" Error reading group ~p for leave_group", [GId]);
        ok ->
            case Group#group.owner of
                UId -> % User is owner, transfer ownership to someone else
                    Members = groups:list_group_members(GId),
                    case Members of
                        [ FirstOne | _ ] ->
                            ok = kvs:put(Group#group{owner = FirstOne}),
                            nsx_msg:notify(["subscription", "user", UId, "remove_from_group"], {GId});
                        [] ->
                            % Nobody left in group, remove group at all
                            nsx_msg:notify([db, group, GId, remove_group], [])
                    end;
                _ -> % Plain user removes -- just remove it
                    nsx_msg:notify(["subscription", "user", UId, "remove_from_group"], {GId})
            end;
        _ -> % user is just someone, remove it
            nsx_msg:notify(["subscription", "user", UId, "remove_from_group"], {GId})
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "subscribe"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): subscribe_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    users:subscribe(UId, Whom),
    {noreply, State};

handle_notice(["subscription", "user", UId, "unsubscribe"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): remove_subscribe: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    users:unsubscribe(UId, Whom),
    {noreply, State};

handle_notice(["subscription", "user", _UId, "update"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): update_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {NewUser} = Message,
    users:update_user(NewUser),
    {noreply, State};

handle_notice(["subscription", "user", Who, "block"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): block_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    users:unsubscribe(Who, Whom),
    case kvs:get(user_ignores, Who) of
        {error, notfound} ->
            kvs:put(#user_ignores{who = Who, whom = [Whom]});
        {ok, #user_ignores{whom = List}} ->
            case lists:member(Whom, List) of
                false ->
                    NewList = [Whom | List],
                    kvs:put(#user_ignores{who = Who, whom = NewList});
                true ->
                    do_nothing
            end
    end,
    case kvs:get(user_ignores_rev, Whom) of
        {error,notfound} ->
            nsx_msg:notify(["db", "user", Whom, "put"], #user_ignores_rev{whom=Whom, who=[Who]});
        {ok,#user_ignores_rev{who=RevList}} ->
            case lists:member(Who, RevList) of
                false ->
                    NewRevList = [Who | RevList],
                    nsx_msg:notify(["db", "user", Whom, "put"], #user_ignores_rev{whom=Whom, who=NewRevList});
                true ->
                    do_nothing
            end
    end,
    nsx_msg:notify_user_block(Who, Whom),
    {noreply, State};

handle_notice(["subscription", "user", Who, "unblock_user"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): unblock_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    List = kvs:list_blocks(Who),
    case lists:member(Whom, List) of
        true ->
            NewList = [ UID || UID <- List, UID =/= Whom ],
            kvs:put(#user_ignores{who = Who, whom = NewList});
        false ->
            do_nothing
    end,
    List2 = kvs:list_blocked_me(Whom),
    case lists:member(Who, List2) of
        true ->
            NewRevList = [ UID || UID <- List2, UID =/= Who ],
            nsx_msg:notify(["db", "user", Whom, "put"], #user_ignores_rev{whom = Whom, who = NewRevList});
        false ->
            do_nothing
    end,
    nsx_msg:notify_user_unblock(Who, Whom),
    {noreply, State};


handle_notice(["gifts", "user", UId, "buy_gift"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): buy_gift: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    users:buy_gift(UId, GId),
    {noreply, State};

handle_notice(["gifts", "user", UId, "give_gift"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): give_gift: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    users:give_gift(UId, GId),
    {noreply, State};

handle_notice(["gifts", "user", UId, "mark_gift_as_deliving"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): mark_gift_as_deliving: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, GTimestamp} = Message,
    users:mark_gift_as_deliving(UId, GId, GTimestamp),
    {noreply, State};


handle_notice(["login", "user", UId, "update_after_login"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): update_after_login: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    Update =
        case users:user_status(UId) of
            {error, status_info_not_found} ->
                #user_status{username = UId,
                             last_login = erlang:now()};
            {ok, UserStatus} ->
                UserStatus#user_status{last_login = erlang:now()}
        end,
    kvs:put(Update),
    {noreply, State};

handle_notice(["invite", "user", UId, "add_invite_to_issuer"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_invite_to_issuer: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {O} = Message,
    kvs:add_invite_to_issuer(UId, O),
    {noreply, State};

handle_notice(["system", "use_invite"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): use_invite: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Code, UId} = Message,
    invite:use_code(Code, UId),
    {noreply, State};

handle_notice(["tournaments", "user", UId, "create"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType} = Message,
    case meetings:create(UId, TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType) of
        {error,X} -> 
            ?ERROR("Error creating tournament: ~p", X);
        TId -> skip
    end,
    {noreply, State};

handle_notice(["tournaments", "user", UId, "create_and_join"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_and_join: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType} = Message,
    case meetings:create(UId, TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType) of
        {error,X} -> 
            ?ERROR("Error creating tournament: ~p", X);
        TId -> 
            meetings:join(UId, TId)
    end,
    {noreply, State};

handle_notice(["likes", _, _, "add_like"] = Route,  % _, _ is here beacause of the same message used for comet update
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_like: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, E} = Message,
    {EId, FId} = E#entry.id,
    feed:add_like(FId, EId, UId),
    {noreply, State};

handle_notice(["personal_score", "user", UId, "add"] = Route,
    Message, #state{owner = Owner, type = Type} = State) ->
    ?INFO("queue_action(~p): personal_score add: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Games, Wins, Loses, Disconnects, Points, AverageTime} = Message,
    scoring:add_personal_score(UId, Games, Wins, Loses, Disconnects, Points, AverageTime),
    {noreply, State};

handle_notice(["system", "add_package"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_package: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {MP} = Message,
    case membership_packages:add_package(MP) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR("Unable to add membership package: ~p, Reason ~p", [MP, Reason])
    end,
    {noreply, State};

handle_notice(["purchase", "user", _, "set_purchase_state"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): set_purchase_state: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),  
    {MPId, NewState, Info} = Message,
    membership_packages:set_purchase_state(MPId, NewState, Info),
    {noreply, State};

handle_notice(["purchase", "user", _, "add_purchase"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_purchase: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    {MP} = Message,
    membership_packages:add_purchase(MP),
    {noreply, State};

handle_notice(["transaction", "user", User, "add_transaction"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_transaction: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    MP = Message,
    kvs:add_transaction_to_user(User,MP),
    {noreply, State};

handle_notice(["purchase", "user", _, "set_purchase_external_id"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): set_purchase_external_id: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {PurchaseId, TxnId} = Message,
    membership_packages:set_purchase_external_id(PurchaseId, TxnId),
    {noreply, State};

handle_notice(["purchase", "user", _, "set_purchase_info"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): set_purchase_info: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {OrderId, Info} = Message,
    membership_packages:set_purchase_info(OrderId, Info),
    {noreply, State};

handle_notice(["system", "tournament_join"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): tournament_join: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, TId} = Message,
    meetings:join(UId, TId),
    {noreply, State};

handle_notice(["system", "tournament_remove"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): tournament_remove: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, TId} = Message,
    meetings:remove(UId, TId),
    {noreply, State};

handle_notice(Route, Message, #state{owner = User} = State) ->
    ?DBG("feed(~p): unexpected notification received: User=~p, "
              "Route=~p, Message=~p", [self(), User, Route, Message]),
    {noreply, State}.

handle_info({feed_refresh, Fid,  Page}, State) -> Reply = kvs:entries_in_feed(Fid,Page), {noreply, State#state{cached_feed=Reply}};
handle_info({direct_refresh, Fid, Page}, State) -> Reply = kvs:entries_in_feed(Fid,Page), {noreply, State#state{cached_direct=Reply}};
handle_info(_Info, State) -> {noreply, State}.

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

remove_entry(#state{feed = Feed, direct = Direct}, EntryId) ->
    [feeds:remove_entry(FId, EntryId) || FId <- [Feed, Direct]].

edit_entry(#state{feed=Feed, direct = Direct}, EntryId, NewDescription) ->
    [feeds:edit_entry(FId, EntryId, NewDescription) || FId <- [Feed, Direct]].


add_comment(#state{feed = Feed, direct = Direct}, From, EntryId, ParentComment,
            CommentId, Content, Medias) ->
    [feeds:entry_add_comment(FId, From, EntryId, ParentComment,
                            CommentId, Content, Medias)
       || FId <- [Feed, Direct]].


add_to_group(UId, GId, Type) ->
    ?INFO("U:~p G:~p T:~p",[UId, GId, Type]),
    case kvs:get(group_subs, {UId, GId}) of
        {error, notfound} ->
            {R, Group} = kvs:get(group, GId),
            case R of 
                error -> ?INFO("Add to group failed reading group");
                _ ->
                    GU = Group#group.users_count,
                    kvs:put(Group#group{users_count = GU+1})
            end;
        _ ->
            ok
    end,
    OK = kvs:put({group_subs,UId,GId,Type,0}),
    ?INFO("RES:~p",[OK]).

remove_from_group(UId, GId) ->
    kvs:delete(group_subs, {UId, GId}),
    {R, Group} = kvs:get(group, GId),
    case R of
        error -> ?INFO("Remove ~p from group failed reading group ~p", [UId, GId]);
        _ ->
            GU = Group#group.users_count,
            kvs:put(Group#group{users_count = GU-1})
    end.


