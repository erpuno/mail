-module(input).
-compile(export_all).
-compile({parse_transform, shen}).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-include("input.hrl").
-include("feed.hrl").

-jsmacro([selectpicker/0, htmlbox/0]).

selectpicker()-> P = jq(".selectpicker"), P:each(fun()-> S = jq(this), S:selectpicker(S:data()) end).
htmlbox() -> H = jq("[data-edit=\"htmlbox\"]"), H:each(fun()-> E = jq(this), E:htmlbox(E:data()) end).

-record(struct, {lst=[]}).

render_element(#input{state=S}) ->
    Source = [S#input_state.title_id,
        S#input_state.body_id,
        S#input_state.recipients_id,
        S#input_state.price_id,
        S#input_state.currency_id,
        S#input_state.scope_id],

    Medias = case wf:cache(medias) of
        undefined when S#input_state.medias /= undefined -> S#input_state.medias;
        undefined -> [];
        Ms -> Ms end,

    wf:render(#panel{id=S#input_state.id, body=[

        #panel{id=S#input_state.toolbar_id, class=[row, "row-fluid"], body=[
            #panel{style=if S#input_state.collapsed -> ""; true -> "display:none;" end,
                    class=[span12,"col-sm-12", "btn-toolbar", S#input_state.class],
                    body=[#link{class=S#input_state.expand_class,
                                body=S#input_state.expand_btn,
                                postback={show_input,S}, delegate=input}]} ]},

        #panel{id=S#input_state.form_id, class=[S#input_state.class],
                style=if S#input_state.collapsed -> "display:none;"; true -> "" end, body=[

            #panel{id=S#input_state.alert_id},
            #h4{body=S#input_state.control_title},

            #panel{class=[row, "row-fluid"], body=[
                #panel{class=[span9,"col-sm-9", input], body=[recipients(S), title(S), body(S), price(S), scope(S)]},
                #panel{class=[span3,"col-sm-3"], body=upload(S)}]},

            #panel{class=[row, "row-fluid"], body=[
                #panel{class=[span9,"col-sm-9", "btn-toolbar"], body=[
                    #button{id=S#input_state.post_id,
                          class=S#input_state.post_class,
                          body=if S#input_state.update -> S#input_state.update_btn; true -> S#input_state.post_btn end,
                          postback={if S#input_state.update -> update; true -> post end, S#input_state.entry_type, S},
                          source=Source, delegate=input},
                    #button{class=S#input_state.close_class,
                          style=if S#input_state.collapsed -> undefined; true -> "display:none;" end,
                          body=S#input_state.close_btn,
                          postback={hide_input, S}, delegate=input},

                    #button{class=S#input_state.cancel_class,
                          style=if S#input_state.update -> undefined; true -> "display:none;" end,
                          body=S#input_state.cancel_btn,
                          postback={flush_input, S}, delegate=input}
                ]},
                #panel{class=[span3,"col-sm-3"]}
            ]},
            #panel{id=S#input_state.media_id, class=[row, "row-fluid"], body=[media_preview(S#input_state.media_id, Medias)]}
        ]} ]}).

recipients(#input_state{show_recipients=true}=S)->
    #textboxlist{id=S#input_state.recipients_id,
        placeholder=S#input_state.placeholder_rcp,
        delegate=input,
        values=S#input_state.recipients,
        role=S#input_state.role};
recipients(_)-> [].

title(#input_state{show_title=true}=S)->
    #textbox{id=S#input_state.title_id,
        class=[span12,"col-sm-12", "form-control"],
        placeholder= S#input_state.placeholder_ttl,
        value=S#input_state.title};
title(_)-> [].

body(#input_state{show_body=true}=S)->
    Root = ?ROOT,
    Dir = ?DIR(case wf:user() of undefined -> "anonymous"; #user{email=E}->E end),

    #htmlbox{id=S#input_state.body_id, class=[span12,"col-sm-12 form-control"],
        html = S#input_state.description,
        root=Root, dir=Dir,
        post_write=attach_media,
        delegate_api=input,
        img_tool=gm,
        post_target=S#input_state.media_id, size=?THUMB_SIZE};
body(_)-> [].

price(#input_state{show_price=true}=S)-> 
    Val = wf:to_list(if S#input_state.price == undefined -> 0; true -> S#input_state.price end/100, [{decimals, 2}]),
    #panel{class=["input-group"], body=[
        #textbox{id=S#input_state.price_id, class=["form-control"],value= Val},
        #panel{class=["input-group-btn"],body=[
            #select{id=S#input_state.currency_id, class=[selectpicker],
                body=[#option{label= L, body = V} || {L,V} <- ?CURRENCY]}
        ]} ]};
price(_)-> [].

scope(#input_state{show_scope=true}=S) -> [
    #select{id=S#input_state.scope_id, class=["form-control"], body=[
        #option{label= <<"scope">>,   body = <<"scope">>, disabled=true, selected=true, style="display:none; color:gray;"},
        #option{label= <<"Public">>,  value = public},
        #option{label= <<"Private">>, value = private} ]} ];
scope(_)-> [].

upload(#input_state{show_upload=true}=S) -> [
    case  S#input_state.upload_title of undefined -> [];
        Title -> #panel{class=["btn-toolbar"], body=#span{class=["btn-toolbar-title"], body=Title}} end,
    #upload{id=S#input_state.upload_id,
        preview= false, root=?ROOT,
        dir=S#input_state.upload_dir,
        delegate_query= S#input_state.delegate_query,
        post_write = S#input_state.post_upload,
        delegate_api = S#input_state.delegate_api,
        img_tool=S#input_state.img_tool,
        post_target=S#input_state.media_id,
        size=?THUMB_SIZE}];
upload(_)-> [].

media_preview(Id, Medias)->
    case Medias of [] -> [];
    _ when length(Medias) > 4 ->
        #carousel{class=[span9,"col-sm-9"], indicators=false, style="border:1px solid #eee;", 
            caption= <<"attachments">>,
            items=[
        #panel{class=[row, "row-fluid"], body=[
            #panel{class=[span3,"col-sm-3"], style="position:relative;", body=[
                #link{class=[close, "text-error"],
                      style="position:absolute; right:10px;top:5px;",
                      body= <<"&times;">>, 
                      postback={remove_media, M, Id},
                      delegate=input},

                #entry_media{media=M, mode=input} ]}
            || M <- lists:sublist(Medias, I, 4) ]}
        || I <- lists:seq(1, length(Medias), 4) ]};
    _ ->
        #panel{class=[row, "row-fluid"], body=[
            begin
            ColId = integer_to_list(12 div length(Medias)),
            #panel{class=["col-sm-"++ColId, "span"++ColId], style="position:relative;", body=[
                #link{class=[close, "text-error"],
                      style="position:absolute; right:10px;top:5px;",
                      body= <<"&times;">>, 
                      postback={remove_media, M, Id},
                      delegate=input},
                #entry_media{media=M, mode=input}
                ]}
            end || #media{} = M <- Medias]}
    end.

% Events

control_event(_, {query_file, Root, Dir, File, MimeType,_,Target})->
    Name = binary_to_list(File),
    FileName = filename:join([Root,Dir,Name]),
    {exist, case file:read_file_info(FileName) of {error, _} -> 0;
        {ok, FileInfo} ->
            error_logger:info_msg("Query file info: ~p", [FileInfo]),
            Media = #media{id = element_upload:hash(FileName),
                url = filename:join([Dir,Name]),
                type = {attachment, MimeType},
                thumbnail_url = filename:join([Dir,"thumbnail",Name])},
            wf:cache(medias, [Media]),
            wf:update(Target, media_preview(Target, [Media])),
            FileInfo#file_info.size end};
control_event(Cid, Role) ->
    SearchTerm = wf:q(term),
    Entries = [{wf:to_list(element(#iterator.id, E)),
        wf:to_list(case Role of
            user-> element(#user.display_name, E);
            product-> element(#product.title, E);
            group-> element(#group.name, E);
        _ -> skip end)}
        || E <- kvs:entries(kvs:get(feed, ?FEED(Role)), Role, undefined)],

    Data = [[list_to_binary(wf:to_list(Role)++Id++"="++Name), list_to_binary(Name)]
        || {Id, Name} <- Entries, string:str(string:to_lower(Name), string:to_lower(SearchTerm)) > 0],
    element_textboxlist:process_autocomplete(Cid, Data, SearchTerm).

event({post, group, #input_state{}=Is}) ->
    User = wf:user(),
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
    Name = wf:q(Is#input_state.title_id),
    Description = escape(wf:q(Is#input_state.body_id)),
    Publicity = case wf:q(Is#input_state.scope_id) of "scope" -> public; undefined -> public; S -> list_to_atom(S) end,
    Id = case Publicity of private -> Name; _ -> kvs:uuid() end,

    Group = #group{id=Id,
                    name = Name,
                    description = Description,
                    scope = Publicity,
                    creator = From,
                    owner = From,
                    feeds = ?GRP_CHUNK,
                    created = now()},

    msg:notify([kvs_group, User#user.email, add], [Group]);

event({post, product, #input_state{}=Is}) ->
    error_logger:info_msg("[input] => save product"),
    User = wf:user(),

    Product = (to_product(Is))#product{
        id = kvs:uuid(),
        creator = User#user.email,
        owner = User#user.email,
        feeds = ?PRD_CHUNK,
        created = now() },

    Groups = groups(Is),

    msg:notify([kvs_product, User#user.email, add], [Product, Groups]);

event({post, comment, #input_state{}=Is}) ->
    wf:info("[input] => comment entry:"),
    Comment = escape(wf:q(Is#input_state.body_id)),
    Medias = case wf:cache(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,

    R2 = [{user, From, {{Is#input_state.entry_id, ?FEED(entry)}, ?FEED(comment)}}],
    Recipients = lists:flatten([Is#input_state.recipients,R2]),

    Cid = kvs:uuid(),
    C = #comment{from = From,
                 content = Comment,
                 media = Medias,
                 created = now()},

    [msg:notify([kvs_feed, Route, To, comment, Cid, add],
        [C#comment{ comment_id = Cid,
                    entry_id   = Eid,
                    feed_id    = CmFid,
                    id         = {Cid, Eid, CmFid},
                    feeds=[{comments, kvs_feed:create()}]}])
        || {Route, To, {Eid, CmFid}} <- Recipients];

event({post, EntryType, #input_state{}=Is})->
    wf:info("[input] => post entry: ~p", [EntryType]),
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
    Eid = case Is#input_state.entry_id of undefined -> kvs:uuid(); Id -> Id end,
    Entry = (to_entry(Is))#entry{
        entry_id=Eid,
        from=From,
        type=EntryType,
        shared="",
        created = now()},
    Recipients = to_recipients(Is,EntryType),

    [msg:notify([kvs_feed, Route, To, entry, Eid, add],
                [Entry#entry{id={Eid, Fid},feed_id=Fid,to={Route, To}}])
        || {Route, To, {_, Fid}} <- Recipients];

event({update, product, Is}) ->
    wf:info("[input]=> update product"),
    Product =to_product(Is),
    Groups = groups(Is),

    msg:notify([kvs_product, Is#input_state.entry_id, update], [Product, Groups]);

event({update, EntryType, Is}) ->
    wf:info("[input]=> update entry"),
    Eid = Is#input_state.entry_id,
    Entry = to_entry(Is),
    Recipients = to_recipients(Is, EntryType),

    Updated = ordsets:from_list([{T,I,F} || {T,I,{_,F}} <- Recipients]),
    Participate = ordsets:from_list([{T,I,F}|| #entry{to={T,I},feed_id=F} <- kvs:all_by_index(entry, entry_id, Eid)]),
    Intersection = ordsets:intersection(Updated, Participate),
    Leave = ordsets:subtract(Participate, Intersection),
    Join = ordsets:subtract(Updated, Intersection),

    [msg:notify([kvs_feed, To, entry, delete],
                [Entry#entry{id={Eid, Fid}, entry_id=Eid, feed_id=Fid, to = {Route, To}}]) 
        || {Route, To, Fid} <- Leave],

    [msg:notify([kvs_feed, Route, To, entry, Eid, add],
                [Entry#entry{id={Eid, Fid}, entry_id=Eid, feed_id=Fid, to = {Route, To}}]) 
        || {Route, To, Fid} <- Join],

    [msg:notify([kvs_feed, Route, To, entry, Eid, edit],
                [Entry#entry{id={Eid, Fid}, entry_id=Eid, feed_id=Fid, to = {Route, To}}])
        || {Route, To, Fid} <- Intersection];

event({remove_media, M, Id}) ->
  New = lists:filter(fun(E)-> E/=M end, case wf:cache(medias) of undefined -> []; Mi -> Mi end),
  wf:cache(medias, New),
  wf:update(Id, media_preview(Id, New));

event({show_input, #input_state{}=S})->
    wf:wire(#jq{target=S#input_state.toolbar_id,method=[hide]}),
    wf:wire(#jq{target=S#input_state.form_id,   method=[fadeIn]});
event({hide_input, #input_state{}=S})->
    wf:wire(#jq{target=S#input_state.form_id,   method=[hide]}),
    wf:wire(#jq{target=S#input_state.toolbar_id,method=[fadeIn]});

event({edit, P=#product{}, #input_state{}=S}) ->
    wf:info("[input] Edit product~p", [S#input_state.id]),
    Groups = string:join([case kvs:get(group, G) of {error,_}-> "";
        {ok, #group{id=Id, name=Name}}-> "group"++wf:to_list(Id)++"="++wf:to_list(Name) end
        || #group_subscription{where=G} <- kvs_group:participate(P#product.id)], ","),

    Is = S#input_state{
        update=true,
        body_id = wf:temp_id(),
        entry_id = P#product.id,
        control_title = <<"Update">>,
        title = P#product.title,
        description = P#product.brief,
        price = P#product.price,
        medias = media(P#product.cover),
        recipients=Groups},

    wf:cache(medias, media(P#product.cover)),
    wf:replace(S#input_state.id, #input{state=Is}),
    wf:wire(selectpicker()),
    wf:wire(htmlbox());

event({edit, #entry{}=E, #input_state{}=S}) ->
    wf:info("[input] Edit entry ~p", [S#input_state.id]),
    Games  = string:join([
        case kvs:get(Type,Id) of {error,_} -> "";
        {ok, #product{id=Pid,title=Title}} -> "product"++wf:to_list(Pid)++"="++wf:to_list(Title) end ||
        #entry{to={Type,Id}} <- kvs:all_by_index(entry, entry_id, E#entry.entry_id), Type==product], ","),

    Is = S#input_state{
        update=true,
        body_id = wf:temp_id(),
        entry_id = E#entry.entry_id,
        control_title = <<"Update">>,
        title = E#entry.title,
        description = E#entry.description,
        medias = E#entry.media,
        recipients=Games},

    wf:cache(medias, E#entry.media),
    wf:replace(S#input_state.id, #input{state=Is}),
    wf:wire(htmlbox());

event({flush_input, #input_state{}=S}) ->
    wf:info("[input] flush_input ~p ~p", [S#input_state.id, (wf_context:context())#context.module]),
    Id = {S#input_state.id, (wf_context:context())#context.module},
    Init = wf:cache(Id),
    Upd = Init#input_state{body_id=wf:temp_id(), update=false},
    wf:update(S#input_state.id, #input{state=Upd}),
    wf:cache(Id, Upd),
    wf:cache(medias, undefined),
    wf:wire(selectpicker()),
    wf:wire(htmlbox());

event(_) -> ok.

api_event(attach_media, Args, _Tag)->
    Props = n2o_json:decode(Args),
    Target = binary_to_list(proplists:get_value(<<"preview">>, Props#struct.lst)),
    Id = proplists:get_value(<<"id">>, Props#struct.lst),
    File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)),
    Type = proplists:get_value(<<"type">>, Props#struct.lst),
    Thumb = binary_to_list(proplists:get_value(<<"thumb">>, Props#struct.lst)),
    Media = #media{id = Id,
        url = File,
        type = {attachment, Type},
        thumbnail_url = Thumb},
    Medias = case wf:cache(medias) of undefined -> []; M -> M end,
    NewMedias = [Media | Medias],
    wf:cache(medias, NewMedias),
    wf:update(Target, media_preview(Target, NewMedias)),
    wf:wire("Holder.run();");

api_event(attach_file, Args, _Tag)->
    Props = n2o_json:decode(Args),
    Target = binary_to_list(proplists:get_value(<<"preview">>, Props#struct.lst)),
    Id = proplists:get_value(<<"id">>, Props#struct.lst),
    File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)),
    Type = proplists:get_value(<<"type">>, Props#struct.lst),

    Url = case string:rstr(File,?ROOT) of 0 -> File;
        Pos -> string:substr(File, Pos+length(?ROOT)) end,

    Media = #media{id = Id,
        url = Url,
        type = {attachment, Type}},
    wf:cache(medias, [Media]),
    wf:update(Target, media_preview(Target, [Media])).

% Utils

to_product(#input_state{}=Is)->
    Title = escape(wf:q(Is#input_state.title_id)),
    Descr = escape(wf:q(Is#input_state.body_id)),
    Price = to_price(wf:q(Is#input_state.price_id)),
    Currency = wf:q(Is#input_state.currency_id),
    Medias = case wf:cache(medias) of undefined -> []; L -> L end,
    Cover = case Medias of [] -> undefined; [#media{url=Url}|_] ->
        case string:rstr(Url,?ROOT) of 0 -> Url;
        Pos -> string:substr(Url, Pos+length(?ROOT)) end end,

    #product{
        title = Title,
        brief = Descr,
        cover = Cover,
        price = Price,
        currency = Currency}.

to_entry(#input_state{}=Is)->
    {Title, Desc} = if Is#input_state.collect_msg == true ->
        {escape(wf:q(Is#input_state.title_id)), escape(wf:q(Is#input_state.body_id))};
    true -> {Is#input_state.title, Is#input_state.description} end,

    Medias = case wf:cache(medias) of undefined -> []; L -> L end,

    #entry{
        media=Medias,
        title=Title,
        description=Desc}.

to_recipients(#input_state{}=Is, EntryType)->
    R1 = if Is#input_state.show_recipients == true ->
        Raw = string:tokens(wf:q(Is#input_state.recipients_id), ","),

        lists:flatmap(fun(S) ->
            Types = [A || A <- ["user", "group", "product"], string:str(S,A) == 1],
            [begin
                Type = list_to_atom(A),
                ObjId = string:tokens(string:substr(S, length(A)+1), "="),
                Feed = case EntryType of review -> reviews; T -> T end,
                case kvs:get(Type, lists:nth(1,ObjId)) of {error,_}-> [];
                {ok, O} -> {Type, element(#iterator.id, O), lists:keyfind(Feed,1,element(#iterator.feeds, O))} end
             end || A <- Types] end, Raw);
    true ->
        Is#input_state.recipients end,

    if R1 /= [] -> wf:info("R1: ~p", [R1]); true -> ok end,

    R2 = lists:filtermap(fun({Route,To,_}) -> case Route of
        product when EntryType==reviews orelse EntryType==review ->
            {true, [{product, To, {feed, ?FEED(entry)}}]++
                [case kvs:get(group, Group) of {error,_} -> [];
                {ok, #group{id=Id,feeds=Feeds}} -> {group, Id, lists:keyfind(feed,1,Feeds)}
                end || #group_subscription{where=Group} <- kvs_group:participate(To)]}; _ -> false end end, R1),

    if R2 /= [] -> wf:info("R2: ~p", [R2]); true -> ok end,

    R3 = case wf:user() of undefined -> [];
        #user{email=Email, feeds=Feeds} ->
            Feed = case EntryType of review -> feed; direct -> sent; reviews -> feed; E -> E end,
            case lists:keyfind(Feed, 1, Feeds) of false -> []; {_,Fid} -> [{user, Email, {Feed,Fid}}] end end,

    if R3 /= [] -> wf:info("R3: ~p", [R3]); true -> ok end,

    lists:flatten([R1,R2,R3]).

groups(#input_state{}=Is)->
    RawRecipients = if Is#input_state.show_recipients == true -> wf:q(Is#input_state.recipients_id);
        true -> Is#input_state.recipients end,

    lists:filtermap(fun(S) -> A = "group",
        case string:str(S, A) of 1 -> {true, {list_to_atom(A),string:substr(S, length(A)+1)}}; _-> false end end,
        string:tokens(RawRecipients, ",")).

media(undefined)-> [];
media(File)-> [#media{url = File,
    thumbnail_url = filename:join([filename:dirname(File),"thumbnail",filename:basename(File)])}].

escape(Input) when is_list(Input) -> escape(list_to_binary(Input));
escape(Input) when is_binary(Input) ->
    R = [{"\r","\n"}, {"\n *",""}, {"^\\s*", ""}, {"^\n*", ""}, {"\n+$",""}, {" +"," "}],
    lists:foldl(fun({Pt, Re}, Subj) ->
        re:replace(Subj, Pt, Re, [global, {return, binary}]) end, Input, R).

to_price(Str)->
    PriceStr2 = case string:to_float(Str) of
        {error, no_float} -> Str;
        {F, _} -> wf:to_list(F, [{decimals, 2}]) end,
    {P1, Rest} = case string:to_integer(PriceStr2) of
        {error, no_integer} -> {0, "0"};
        {Pa, [_|Ra]} -> {Pa, Ra};
        {Pa, Ra} -> {Pa, Ra} end,
    P2 = case string:to_integer(Rest) of
        {error,no_integer}-> 0;
        {Re,_} -> Re  end,
    P2+P1*100.
