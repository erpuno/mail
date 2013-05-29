-module(feed_mailer).
-behaviour(feed_consumer).
-include("log.hrl").
-include_lib("kvs/include/membership_packages.hrl").
-include_lib("kvs/include/config.hrl").
-export([init/1, handle_notice/3, get_opts/1, handle_info/2, start_link/2]).
-record(state, {name,type,smtp_options = []}).

start_link(Mod,Args) -> gen_server:start_link(Mod, Args, []).

init(Params) ->
    SMTPOptions = read_smtp_options(),
    Name = proplists:get_value(name,Params),
    Type = proplists:get_value(type,Params),
    {ok, #state{smtp_options = SMTPOptions,name=Name,type=Type}}.

handle_notice(["email", "send"], {Subject, Content, To}, #state{smtp_options = Options} = State) ->
    ?INFO("email(~p): send email to ~s. Subject:~s", [self(), To, Subject]),
    mail:send(Subject, Content, To, Options),
    {noreply, State};

handle_notice(["email", "send"], {Subject, TextContent, HTMLContent, To},
              #state{smtp_options = Options} = State) ->
    ?INFO("email(~p): send multipart email to ~s. Subject:~s", [self(), To, Subject]),
    mail:send_multipart(Subject, TextContent, HTMLContent, To, Options),
    {noreply, State};

handle_notice(["purchase", User, PurchaseId, PaymentType, PurchaseState],
              #membership_purchase{} = _Purchase,
              #state{smtp_options = Options} = State) ->
    case kvs:get(config, "purchase/notifications/email") of
        [] ->
            ?INFO("email(~p): purchase notifications recipients list is empty",
                 [self()]);

        {ok, #config{value = SendTo}} ->
            Message = io_lib:format(
                        "Purchase ~s state has been changed.\nNew state: ~s.\n"
                        "Payment type: ~s\nUser: ~s",
                        [PurchaseId, PurchaseState, PaymentType, User]),
            Subject = io_lib:format("Purchase ~s state has been changed: ~s",
                            [PurchaseId, PurchaseState]),
            mail:send(lists:flatten(Subject), lists:flatten(Message), SendTo, Options);

        {error, Reason} ->
            ?ERROR("email(~p): unable to get purchase notification recipients: ~p",
                  [self(), Reason])
    end,
    {noreply, State};

handle_notice(Route, Payload, State) ->
    ?INFO("email(~p): notice received. Route: ~p, Payload: ~p",
          [self(), Route, Payload]),
    {noreply, State}.

handle_info(Info, State) ->
    ?INFO("reanimator(~p): handle info: ~p",
          [self(), Info]),
    {noreply, State}.


get_opts(_State) ->
    [{routes, [[email, '*'],
               [purchase, '*', '*', '*', '*']]},
     {queue, <<"notice.email.1">>},
     {queue_options, [auto_delete]}].

read_smtp_options() -> [].
