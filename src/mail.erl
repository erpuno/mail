-module(mail).
-include_lib("feed_server/include/feed_server.hrl").
-include_lib("kvs/include/log.hrl").

-type smtp_options() :: with_ssl | {server, string()} | {port, integer()} |
          {user, string()} | {password, string()}.

-export([send/4,
         send_multipart/5]).

-define(SOCK, case ?SMTP_SSL of
                  true  -> ssl;
                  false -> gen_tcp
              end).


-spec send(string(), string(), string(), smtp_options()) -> ok.

send(Subject, Content, To, Options) ->
    User = nsx_opt:opt(user, Options, ?SMTP_USER),
    SendTo = format_send_to(To),
    ContentToSend = io_lib:format(
                      "Subject: ~s\r\n"
                      "From: Kakaranet.com <~s> \r\n"
                      "To: ~s \r\n"
                      "Content-Transfer-Encoding: 7bit \r\n"
                      "Content-Type: text/plain; charset=\"utf-8\" \r\n"
                      "MIME-Version: 1.0 \r\n\r\n"
                      "~s",
                      [Subject, User, SendTo, Content]),

    send_simple(Subject, ContentToSend, To, Options).

-spec send_multipart(string(), string(), string(), string(), smtp_options()) -> ok.
%% @doc sending both plain and html version according to
%% http://tools.ietf.org/html/rfc2046#section-5.1.4
send_multipart(Subject, TextContent, HTMLContent, To, Options) ->
    User = nsx_opt:opt(user, Options, ?SMTP_USER),
    SendTo = format_send_to(To),
    Boundary = "3b51a38ea77fe850dfba67b3e9192c001b211b3f",
    TS = io_lib:format(
           "From: Kakaranet.com <~s> \r\n" %% From
           "To: ~s \r\n" %% Mail
           "Subject: ~s\r\n" %% Subject
           "MIME-version: 1.0\r\n"
           "Content-Transfer-Encoding: 7bit\r\n"
           "Content-type: multipart/alternative; boundary=\"~s\"\r\n" %% Boundary
           "\r\n"
           "--~s\r\n" %% Boundary
           "Content-Type: text/plain; charset=\"utf-8\"\r\n"
           "\r\n"
           "~s\r\n" %% TextContent
           "\r\n"
           "--~s\r\n" %% Boundary
           "Content-Type: text/html; charset=\"utf-8\"\r\n"
           "\r\n"
           "~s\r\n" %% TextContent
           "\r\n"
           "--~s--", %% Boundary
           [User, SendTo, Subject,
            Boundary, Boundary, TextContent,
            Boundary, HTMLContent, Boundary
           ]),

    send_simple(Subject, TS, To, Options).


send_simple(Subject, Content, To, Options) ->
    WithSSL = nsx_opt:opt(with_ssl, Options, ?SMTP_SSL),
    Server  = nsx_opt:opt(server, Options, ?SMTP_HOST),
    Port    = nsx_opt:opt(port, Options, ?SMTP_PORT),
    User    = nsx_opt:opt(user, Options, ?SMTP_USER),
    Password  = nsx_opt:opt(password, Options, ?SMTP_PASSWD),

    ?INFO("Sibject:To: {~p,~p}", [Subject, To]),

    {ok, SockType, Sock} = connect(WithSSL, Server, Port),
    case Sock of 
         none -> ?INFO("ERROR: Can't send email to: ~p",[To]);
         Socket ->

    ?INFO("Socket: ~p",[Socket]),
    send_data(SockType, Socket, "HELO kakaranet.com"),
    send_data(SockType, Socket, "AUTH LOGIN"),
    send_data(SockType, Socket, base64:encode_to_string(User)),
    send_data(SockType, Socket, base64:encode_to_string(Password)),
    send_data(SockType, Socket, "MAIL FROM: <"++User++">"),
    rcpt_to(SockType, Socket, To),
    send_data(SockType, Socket, "DATA"),
    send_no_receive(SockType, Socket, Content),
    send_no_receive(SockType, Socket, ""),
    send_data(SockType, Socket, "."),
    send_data(SockType, Socket, "QUIT"),
    close(SockType, Socket)

    end,
    ok.


rcpt_to(_, _, []) ->
    ok;
rcpt_to(SocketType, Socket, [[_|_]=To|Rest]) ->
    rcpt_to(SocketType, Socket, To),
    rcpt_to(SocketType, Socket, Rest);
rcpt_to(SocketType, Socket, To) ->
    send_data(SocketType, Socket, "RCPT TO: <" ++ To ++">").




connect(WithSSL, Server, Port) ->
    SockType = case WithSSL of
                   true ->
                       ssl;
                   false ->
                       gen_tcp
               end,
    {ok, Socket} = case SockType:connect(Server, Port, [{active, false}], 140000) of
                        {ok,S} -> recv(SockType, S), {ok,S};
                        {error,_} -> {ok, none} end,
    {ok, SockType, Socket}.

send_data(SockType, Socket, Data) ->
    ?INFO("ME: ~p", [Data]),
    SockType:send(Socket, Data ++ "\r\n"),
    recv(SockType, Socket).

send_no_receive(SockType, Socket, Data) ->
    ?INFO("ME: ~p", [Data]),
    SockType:send(Socket, Data ++ "\r\n").

close(SockType, Socket) ->
    SockType:close(Socket).


recv(SockType, Socket) ->
    case SockType:recv(Socket, 0, 140000) of
        {ok, Return}    -> ?INFO("SERVER: ~p", [Return]);
        {error, Reason} -> ?INFO("ERROR: ~p",  [Reason])
    end.

%% when To - is list of emails
format_send_to([[_|_]|_] = To) ->
    string:join(To, ",");
format_send_to(To) ->
    To.
