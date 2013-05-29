-module(feed_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,pid/1]).
-include_lib("feed_server/include/feed_server.hrl").
-record(state,{}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    {ok, Channel} = mqs:open([]),
    mqs_channel:create_exchange(Channel, ?DEAD_LETTER_EXCHANGE,
                                   [{type, <<"fanout">>}, durable, {auto_delete, false}]),

    case nsx_opt:get_env(feed_server,start_email,false) of
         false -> skip;
         true ->  feed_sup:start_worker(feed_mailer, [{name,"@mailer"},{type,system}])
    end,

    {ok, BPid} = feed_sup:start_worker(feed_launcher, [{name, "@launcher"},{type,system}]),
    erlang:send(BPid,start_all),

    {ok,#state{}}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

pid(Name) -> 
    R=[Pid||X={{p,l,SName},Pid,Value}<-qlc:e(gproc:table()),Name==SName],
    case R of [] -> undefined; [A] -> A; _ -> ambiguous end.
