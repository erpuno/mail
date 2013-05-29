-module(nsm_consumer).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-behaviour(gen_server).
-include_lib("msq/include/mqs.hrl").
-include("log.hrl").
-include("feed_server.hrl").
-export([behaviour_info/1,delivery/2,start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
behaviour_info(callbacks) ->
    [{handle_notice, 3}, %% handle queue delivery
     {handle_info, 2},   %% handle erlang message delivery
     {init, 1},
     {get_opts, 1}];
behaviour_info(_) -> undefined.

-record(state, {callback_state, callback, channel, callback_queue, system_queue, channel_mon, gproc_name}).

-define(DEFAULT_WORKER_QUEUE_OPTIONS, [auto_delete]).
-define(DEFAULT_WORKER_CONS_OPTIONS,  []).

%% options for subscription for system notifications
%% FIXME: check, delete if don't need this stuff
-define(SYSTEM_SUB_OPTS, [{routes, [[notice, system, reconfigure]]},
                          {queue, <<"">>},
                          {queue_options, [auto_delete]}]).

start_link(Module, Args) -> gen_server:start_link(?MODULE, [Module|Args], []).
delivery(Envelope, Server) -> gen_server:cast(Server, {delivery, Envelope}).

init([Module | Args]) ->
    process_flag(trap_exit, true),

    case catch Module:init(Args) of
        {ok, CState} ->
            {ok, Channel} = mqs:open([]),

            MonRef = erlang:monitor(process, Channel),
            Options = Module:get_opts(CState),

            {ok, CallbackQueue} = subscribe_client(Channel, Options), % subscribe for client's events
            {ok, SystemQueue} = subscribe_system(Channel), % subscribe for system events

            %% register in gproc if gproc name specified
%            GProcName = proplists:get_value(gproc_name, Options),
%            case GProcName of
%                undefined ->
%                    ok;
%                Name ->
%                    ?INFO("register ~p in gproc with name ~p", [Module, Name]),
%                    catch gproc:reg(gproc_key(Name), Module)
%            end,

            {ok, #state{callback_state = CState,
                        callback       = Module,
                        channel        = Channel,
                        system_queue   = SystemQueue,
                        callback_queue = CallbackQueue,
                        channel_mon    = MonRef}};
%                        gproc_name     = GProcName}};

        {stop, Reason} ->    {stop, Reason};
        {'EXIT', Reason} ->  {stop, {init_failed, {Module, Reason}}}
    end.

handle_call(Request, _From, State) ->
    {Reply, Answer, NewState} = nsm_writer:handle_call(Request,_From,State#state.callback_state),
    {reply, Answer, State#state{callback_state=NewState}}.

handle_cast({delivery, Envelope}, State) ->
    %% transform mq fromat to pass to callback module
    Route   = mqs_lib:key_to_list(Envelope#envelope.routing_key),
    process_delivery(Route, Envelope, State).

handle_info({'DOWN', MRef, process, Ch, Reason}, #state{channel = Ch, channel_mon = MRef, callback_state = CS} = State) ->
    ?ERROR("channel failed: ~p. CallbackState: ~p. Restart.", [Reason, CS]),
    {stop, {channel_failed, Ch}, State};

handle_info(Info, #state{callback = Module, callback_state = CState0} = State)->
    process_result(State, Module:handle_info(Info, CState0));

handle_info(Info, State) ->
    ?WARNING("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.gproc_name of
        undefined -> ok;
        Name -> gproc:unreg(gproc_key(Name))
    end.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

process_delivery(["notice", "system", "reconfigure"], #envelope{payload = Conf}, State) ->
%    ?INFO("recofigure received: ~p", [Conf]),
    {noreply, State};
process_delivery(Route, Envelope, #state{callback = Module, callback_state = CState0} = State) ->
%    ?DBG("process delivery. Not system call: ~p", [Route]),
    Payload = Envelope#envelope.payload,
    process_result(State, catch Module:handle_notice(Route, Payload, CState0)).

process_result(State, Result) ->
    Module  = State#state.callback,
    Channel = State#state.channel,
    case Result of
        {'EXIT', Reason} -> ?ERROR("~w: error: ~p, Stack: ~p", [Module, Reason, erlang:get_stacktrace()]), {noreply, State};
        {noreply, CState1} -> {noreply, State#state{callback_state = CState1}};
        {send, SRoutingKey, SMessage, CState1} -> publish(Channel, ?NOTIFICATIONS_EX, SRoutingKey, SMessage),
                                                  {noreply, State#state{callback_state = CState1}};
        {send, SExchange, SRoutingKey, SMessage, CState1} ->  publish(Channel, SExchange, SRoutingKey, SMessage),
                                                              {noreply, State#state{callback_state = CState1}};
        {stop, Reason, CState1} -> {stop, Reason, State#state{callback_state = CState1}};
        Unexpected ->              {stop, Unexpected, State}
    end.

subscribe_client(Channel, SubsOptions) ->
    Routes    = proplists:get_value(routes, SubsOptions),
    Queue     = proplists:get_value(queue,  SubsOptions),
    Exchange  = proplists:get_value(exchange,  SubsOptions, ?NOTIFICATIONS_EX),
    QueueOpts = proplists:get_value(queue_options, SubsOptions, ?DEFAULT_WORKER_QUEUE_OPTIONS),
    ConsumeOpts = proplists:get_value(consume_options, SubsOptions, ?DEFAULT_WORKER_CONS_OPTIONS),
    subscribe(Channel, Exchange, Routes, Queue, QueueOpts, ConsumeOpts).

%% subscribe for notice system messages (reconfigure, etc.)
subscribe_system(Channel) ->
    Routes    = proplists:get_value(routes, ?SYSTEM_SUB_OPTS),
    Queue     = proplists:get_value(queue,  ?SYSTEM_SUB_OPTS),
    QueueOpts = proplists:get_value(queue_options, ?SYSTEM_SUB_OPTS),
    subscribe(Channel, ?NOTIFICATIONS_EX, Routes, Queue, QueueOpts, []).


subscribe(Channel, Exchange, Routes, Queue, QueueOptions, ConsumeOptions) ->
%    ?INFO("gen_worker: ~p, Exchange: ~p, Routes: ~p, Queue:~p, QueueOptions: ~p", [Channel, Exchange, Routes, Queue, QueueOptions]),
    {ok, _} = nsm_mq_channel:create_queue(Channel, Queue, QueueOptions),
    [begin RoutingKey = nsm_mq_lib:list_to_key(Route), 
           ok = nsm_mq_channel:bind_queue(Channel, Queue, Exchange, RoutingKey) end || Route <- Routes],
    ConsumeOpts = [{callback, {?MODULE, delivery}}, {state, self()}|ConsumeOptions],
    {ok, _Ctag} = nsm_mq_channel:consume(Channel, Queue, ConsumeOpts),
    {ok, Queue}.

publish(Channel, Exchange, RoutingKey, Message) ->
    BinaryRoutingKey = case RoutingKey of
                            L when is_list(L)-> nsm_mq_lib:list_to_key(L);
                            B when is_binary(B) -> B;
                            _ -> throw({unexpected_routing_key_format, RoutingKey})
    end,
    ok = nsm_mq_channel:publish(Channel, Exchange, BinaryRoutingKey, Message).

gproc_key(Name) -> {p, l, Name}.
