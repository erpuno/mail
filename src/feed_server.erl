-module(feed_server).
-behaviour(gen_server).
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/accounts.hrl").
-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-record(state,{}).
-define(SYSTEM_FEEDS, [{feed,?USR_FEED},{feed,?PRD_FEED},{feed,?GRP_FEED},{feed,?ENT_FEED},{feed,?CMT_FEED}]).

start_link() -> case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
                    {ok,Pid} -> Pid ! start_all;
                    E -> error_logger:info_msg("[feed_server] Start failed! ~p", [E]) end.

init([])->
    gproc:reg({p,l,?MAIN_CH}),
    kvs:wait_for_tables(),
    {ok, #state{}}.

handle_call(_,_,S) -> {reply, ok, S}.
handle_cast(_,S) -> {noreply, S}.
handle_info({delivery, Route, Msg}, S) -> handle_notice(Route, Msg), {noreply, S};
handle_info(start_all, S) ->
    error_logger:info_msg("Starting workers..."),

    [handle_notice([ok, user,    init], [Id, Feeds]) || #user{email=Id, feeds=Feeds} <- kvs:all(user)],
    [handle_notice([ok, group,   init], [Id, Feeds]) || #group{id = Id, feeds=Feeds} <- kvs:all(group)],
    [handle_notice([ok, product, init], [Id, Feeds]) || #product{id=Id, feeds=Feeds} <- kvs:all(product)],

%    CheckNode = fun(X) -> lists:foldl(fun(A, Sum) -> A + Sum end, 0, X) rem 3 + 1 end,
%    RunGroups = fun(Groups) -> [handle_notice([ok,group,init],[Id, Feeds]) || #group{id=Id,feeds=Feeds}<-Groups] end,
%    RunSystem = fun() -> handle_notice([ok,system,init], ["system", ?SYSTEM_FEEDS]) end,
%    Node = application:get_env(feed_server,pool,5),
%    Users = case Node of
%                 4 -> [User || User<-kvs:all(user), User#user.email /= undefined, User#user.status == ok];
%                 5 -> [User || User<-kvs:all(user), User#user.email /= undefined, User#user.status == ok];
%                 X -> [R||R=#user{email=U,status=ok}<-kvs:all(user), CheckNode(E)==X,E/=undefined]
%    end,
%    error_logger:info_msg("Users Count on Node ~p: ~p",[Node,length(Users)]),
%    Groups = [Group || Group=#group{id=G} <- kvs:all(group), CheckNode(G)==Node ],
%    AllGroups = kvs:all(group),
%    case Node of
%         1 -> RunGroups(Groups);
%         2 -> RunGroups(Groups);
%         3 -> RunGroups(Groups);
%         4 -> RunGroups(AllGroups);
%         5 -> RunGroups(AllGroups);
%         _ -> skip end,
%    [handle_notice([ok,user,init],[Id,Feeds])|| #user{email=Id,feeds=Feeds} <- Users],
%    case Node of
%         1 -> RunSystem();
%         4 -> RunSystem();
%         5 -> RunSystem();
%         _ -> skip end,
%    garbage_collect(self()),
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
