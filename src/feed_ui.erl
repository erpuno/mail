-module(feed_ui).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("feed.hrl").
-include("input.hrl").

-define(DATA_TAB,       [{<<"data-toggle">>,<<"tab">>}]).
-define(TOOLTIP,        [{<<"data-toggle">>,<<"tooltip">>}]).
-define(UPDATE_DOM(Method,Target,Elements,Format),
    wf:wire(#jq{format=Format,target=Target,method=[Method], args=[Elements]})).

feed_state(Id) ->  wf:cache({Id,?CTX#context.module}).
input_state(Id) -> wf:cache({?FD_INPUT(Id),?CTX#context.module}).

render_element(#feed_ui{state=S}=F) ->
    Title = case F#feed_ui.title of undefined -> "";T->T end,
    Icon = F#feed_ui.icon,
    IconUrl = F#feed_ui.icon_url,
    Class= F#feed_ui.class,
    TableHeader = F#feed_ui.header,
    SelectionCtl = F#feed_ui.selection_ctl,
    wf:render(#section{class=[feed, Class], body=[
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_}->
            #panel{id=S#feed_state.feed_title, class=["row-fluid", "feed-title", Class], body=[
                #panel{class=[span1],  body=#h4{body=[#i{class=[Icon]}]}},
                #panel{class=[span11], body=#h4{body=[Title, #span{class=["text-warning"], body= <<" [no feed]">>}]}}]};
        {ok, Feed} ->
            wf:cache(S#feed_state.selected_key,[]),

            Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),

            wf:cache(S#feed_state.visible_key, [element(S#feed_state.entry_id, E) || E <- Entries]),

            Total = element(#container.entries_count, Feed),
            Current = length(Entries),
            {Last, First} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,
            State = S#feed_state{
                start_element = First,
                last_element = Last,
                start = 1,
                total = Total,
                current = Current},

            [
            %% header

            if S#feed_state.show_title == true ->
            #panel{id=S#feed_state.feed_title, class=["row-fluid", "feed-title", Class], body=[
                #panel{class=[span1], body=#h4{body=[
                    case IconUrl of undefined -> #i{class=[Icon]};
                    Url -> #link{url=Url, body=[#i{class=[Icon]}], data_fields=?DATA_TAB} end,
                    % select all element control
                    if S#feed_state.enable_selection == true ->
                        #span{id=S#feed_state.selectall_ctl, body=[
                            #checkbox{id=S#feed_state.select_all, class=[checkbox, inline], 
                                postback={select, S#feed_state.select_all, State},
                                delegate=S#feed_state.delegate_sel,
                                source=[S#feed_state.select_all],
                                value= string:join([wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))) || E <- Entries], "|"),
                                style= if Total > 0 -> [] ; true-> "display:none;" end}]}; true -> [] end]}},

                #panel{class=[span11], body=#h4{body=[
                    if is_atom(Title) == true -> wf:to_list(Title); true -> Title end,
                    if S#feed_state.enable_traverse == false ->
                        #span{id=S#feed_state.page_label, class=["text-warning"], body=if Current == 0 -> <<" [no entries]">>;
                        true -> " [" ++ integer_to_list(Total) ++ "]" end};
                    true -> [] end,
                    #span{id=S#feed_state.alert, class=[]},

                    #span{id=S#feed_state.select_toolbar, style="display:none;", class=["selection-ctl"], body=[
                        #link{id=S#feed_state.delete_btn, class=[btn], body=[#i{class=["icon-trash"]}],
                            data_fields=?TOOLTIP, title= <<"delete">>,
                            postback={delete, State}, delegate=feed_ui},
                        SelectionCtl]},

                    if S#feed_state.enable_traverse == true ->
                        #span{class=["pull-right", "traverse-ctl"], body=[
                            #span{id=S#feed_state.feed_toolbar, body=if Total > 0 -> [
                                #small{id=S#feed_state.page_label, body=[
                                    integer_to_list(State#feed_state.start), "-", integer_to_list(Current), " of ", integer_to_list(Total)]},
                                #button{id=S#feed_state.prev,
                                    disabled = element(#iterator.next, First) == undefined,
                                    class=[btn, case element(#iterator.next, First) of undefined -> "disabled"; _ -> "" end],
                                    body=[#i{class=["icon-chevron-left"]}], data_fields=?TOOLTIP, title= <<"previous">>,
                                    postback={traverse, #iterator.next, First, State}, delegate=feed_ui},
                                #button{id=S#feed_state.next,
                                    disabled = element(#iterator.prev, Last) == undefined,
                                    class=[btn, case element(#iterator.prev, Last)  of undefined -> "disabled"; _ -> "" end],
                                    body=[#i{class=["icon-chevron-right"]}], data_fields=?TOOLTIP, title= <<"next">>,
                                    postback={traverse, #iterator.prev, Last, State}, delegate=feed_ui}];
                            true-> [
                                #small{id=S#feed_state.page_label, body=[#span{class=["text-warning"], body= <<" [no entries]">>}]},
                                #i{id=S#feed_state.prev},#i{id=S#feed_state.next} ] end} ]}; true -> [] end,

                    #span{class=["pull-right"], body=[
                        #link{id=S#feed_state.close, class=[close, "text-error"], postback={cancel_select, State}, delegate=feed_ui, body= <<"&times;">>}
                    ]}
                ]}}
            ]}; true -> [] end,

            %% feed body

            if S#feed_state.html_tag == table ->
                #table{class=[table, "table-hover"], header=if S#feed_state.show_header == true -> [TableHeader]; true -> [] end,
                    body=#tbody{id=S#feed_state.entries, class=["feed-body"], body=[#feed_entry{entry=G, state=State} || G <- Entries]}};
                true -> [if S#feed_state.show_header == true -> TableHeader; true -> [] end,
                    #panel{id=S#feed_state.entries, body=[#feed_entry{entry=G, state=State} || G <- Entries]}] end,

            %% footer

            if S#feed_state.enable_traverse == false ->
                #panel{id=S#feed_state.more_toolbar, class=["btn-toolbar", "text-center"], body=
                    if Current < S#feed_state.page_size -> [];
                    true -> #link{class=[btn, "btn-info"],
                                    body= <<"more">>,
                                    delegate=feed_ui,
                                    postback = {check_more, Last, State}} end}; true -> [] end
            ]
        end ]});

% feed entry representation

render_element(#feed_entry{entry=E, state=S})->
    Id = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    SelId = ?EN_SEL(Id),
    El = wf:render(if S#feed_state.html_tag == table ->
        #tr{id=?EN_ROW(Id), cells=[
            if S#feed_state.enable_selection == true ->
                #td{body= [#checkbox{id=SelId,
                    postback={select, SelId, S},
                    delegate=S#feed_state.delegate_sel,
                    source=[SelId], value=Id}]}; true -> [] end,
            #row_entry{entry=E, state=S, module=S#feed_state.delegate}
        ]};
        true -> #panel{id=?EN_ROW(Id), class=["row-fluid", article], body=[
            if S#feed_state.enable_selection == true -> [
                #panel{class=[span1], body=#checkbox{id=SelId, class=["text-center"],
                    postback={select, SelId, S},
                    delegate=S#feed_state.delegate_sel,
                    source=[SelId], value=Id}},
                #panel{class=[span11, "row-fluid"], body= #div_entry{entry=E, state=S, module=S#feed_state.delegate}}];
            true -> #div_entry{entry=E, state=S, module=S#feed_state.delegate} end
        ]} end),
    if S#feed_state.js_escape -> wf:js_escape(El); true -> El end;

render_element(#row_entry{entry=E, state=#feed_state{}=S}) -> wf:render(#td{body=wf:to_list(element(S#feed_state.entry_id, E))});
render_element(#div_entry{entry=E, state=#feed_state{}=S}) -> wf:render(#td{body=wf:to_list(element(S#feed_state.entry_id, E))});

% Media elements

render_element(#entry_media{media=[], mode=reviews}) ->
    element_image:render_element(image(#media{}, "270x124/auto"));
render_element(#entry_media{media=[]}) -> wf:render([]);
render_element(#entry_media{media=undefined}) -> wf:render([]);
render_element(#entry_media{media=M, mode = input}) ->
    wf:render(#panel{class=[thumbnail], body=case M#media.thumbnail_url of
        undefined ->
            #panel{style="word-wrap:break-word;font-size:small;background:white;", body=M#media.url};
        Th ->
            Ext = filename:extension(Th),
            Img = filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_139x80"++Ext]),
            #image{image= Img} end});
render_element(#entry_media{media=#media{url=undefined}=Media, mode=store}) ->
    element_image:render_element(image(Media, "190x97/auto"));
render_element(#entry_media{media=[#media{}=Media|_]}) ->
    element_image:render_element(image(Media, "270x124"));
render_element(#entry_media{media=#media{}=Media, mode=blog}) ->
    element_image:render_element(image(Media, "716x480"));
render_element(#entry_media{media=#media{}=Media}) ->
    element_image:render_element(image(Media, "270x124"));

render_element(E) -> wf:info("[feed_ui]render unknown ~p", [E]).

image(#media{}=Media, Size) ->
    case Media#media.thumbnail_url of undefined ->
        #image{data_fields=[{<<"data-src">>, "holder.js/"++wf:to_list(Size)++"/text:no media"}], alt=Media#media.title};
        Thumb ->
            Ext = filename:extension(Thumb),
            Name = filename:basename(Thumb, Ext),
            Dir = filename:dirname(Thumb),
            #image{alt=Media#media.title, image=filename:join([Dir, Name++"_"++wf:to_list(Size)++Ext])} end.

% events

event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({traverse, Direction, Start, #feed_state{}=S}) -> traverse(Direction, Start, S);

event({delete, #feed_state{selected_key=Selected, visible_key=Visible}=S}) ->
    Selection = sets:from_list(wf:cache(Selected)),
    User = wf:user(),
    {Type, Module} = case S#feed_state.entry_type of
        product -> {product, kvs_product};
        entry when S#feed_state.view == product -> {product, kvs_product};
        group -> {group, kvs_group};
        T -> {T, kvs_feed} end,

    [begin
        case kvs:get(Type, Id) of {error,_} -> wf:info("[feed_ui] no ~p ~p", [Type, Id]);
        {ok, Obj} ->
        Route = if S#feed_state.del_by_index orelse Type /= entry ->
            [Module, User#user.email, delete];
         true ->
            [Module, User#user.email, Type, delete] end,

        msg:notify(Route, [Obj]) end
    end || Id <- wf:cache(Visible), sets:is_element(wf:to_list(erlang:phash2(Id)), Selection)];

event({cancel_select, #feed_state{}=S}) -> deselect(S);

event({select, Sel, #feed_state{selected_key=Key}=S})->
    Selection = wf:cache(Key),
    NewSel = case wf:q(Sel) of "undefined" -> if Sel == S#feed_state.select_all -> sets:new(); true ->
        SelEn = ?EN_FROMSEL(Sel),
        wf:wire(#jq{target=?EN_ROW(SelEn), method=["removeClass"], args=["'warning'"]}),
        sets:from_list(Selection--[SelEn]) end;
    Val -> Vals = string:tokens(Val,"|"),
        [begin
            wf:wire(#jq{target=?EN_SEL(C), method=["prop"],     args=["'checked', 'checked'"]}),
            wf:wire(#jq{target=?EN_ROW(C), method=["addClass"], args=["'warning'"]})
        end || C <- Vals],
        sets:from_list(Vals++Selection) end,

    case sets:size(NewSel) of 0 -> deselect(S);
    Size ->
        wf:wire(#jq{target=S#feed_state.select_toolbar, method=["show"]}),
        wf:wire(#jq{target=S#feed_state.close,          method=["show"]}),
        wf:wire(#jq{target=S#feed_state.feed_title,     method=["attr"], args=["'style', 'background-color:lightblue'"]}),
        wf:wire(#jq{target=S#feed_state.feed_toolbar,   method=["hide"]}),
        wf:wire(#jq{target=S#feed_state.select_all,     method=["prop"], args=["'checked'," ++
            if Size == S#feed_state.page_size orelse Size == S#feed_state.current -> "'checked'"; true -> "false" end]})
    end,
    wf:cache(Key, sets:to_list(NewSel));

event({check_more, Start, #feed_state{}=S}) ->
    traverse_entries(S#feed_state.entry_type, element(#iterator.prev,Start), S#feed_state.page_size, S),
    wf:update(S#feed_state.more_toolbar, []);

event(_) -> ok.

deselect(#feed_state{selected_key=Key}=S) ->
    [begin
        wf:wire(#jq{target=?EN_SEL(C), method=["prop"], args=["'checked', false"]}),
        wf:wire(#jq{target=?EN_ROW(C), method=["removeClass"], args=["'warning'"]})
     end|| C <- case wf:cache(Key) of undefined -> []; Ims -> Ims end],
    SA = if is_tuple(S#feed_state.container_id) -> ?FD_SELALL(S#feed_state.container_id); true -> S#feed_state.select_all end,
    wf:wire(#jq{target=SA, method=["prop"], args=["'checked', false"]}),
    wf:wire(#jq{target=S#feed_state.select_toolbar, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_toolbar, method=["show"]}),
    wf:wire(#jq{target=S#feed_state.close, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_title, method=["attr"], args=["'style', 'background-color:none;'"]}),
    wf:cache(Key, []).

% Traverse

traverse(Direction, Start, #feed_state{}=S)->
    {ok, Container} = kvs:get(S#feed_state.container, S#feed_state.container_id),
    Top = element(#container.top, Container),
    Entries = case element(Direction, Start) of
        undefined  -> kvs:entries(Container, S#feed_state.entry_type, S#feed_state.page_size);
        Prev -> kvs:entries(S#feed_state.entry_type, Prev, S#feed_state.page_size, Direction)
    end,

    wf:cache(S#feed_state.visible_key, [element(S#feed_state.entry_id, E)|| E<-Entries]),

    {NewLast, NewFirst} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,
    Total = element(#container.entries_count, Container),
    Current = length(Entries),

    NewStart = case Direction of
        #iterator.prev -> S#feed_state.start + S#feed_state.page_size;
        #iterator.next -> if Top==element(#iterator.id, Start) -> 1; true -> S#feed_state.start-S#feed_state.page_size end end,

    State = S#feed_state{
        start=NewStart,
        start_element=NewFirst,
        last_element=NewLast,
        current=Current},

    wf:update(S#feed_state.entries, [#feed_entry{entry=G, state=State#feed_state{js_escape=true}} || G <- Entries]),

    if Total == 0 ->
        wf:replace(S#feed_state.prev, #i{id=S#feed_state.prev}),
        wf:replace(S#feed_state.next, #i{id=S#feed_state.next});
    true ->
        wf:replace(S#feed_state.prev, #button{id=State#feed_state.prev,
            disabled = element(#iterator.next, NewFirst) == undefined,
            class=[btn, case element(#iterator.next, NewFirst) of undefined -> "disabled"; _ -> "" end],
            body=[#i{class=["icon-chevron-left"]}], data_fields=?TOOLTIP, title= <<"previous">>,
            postback={traverse, #iterator.next, NewFirst, State}, delegate=feed_ui}),
        wf:replace(S#feed_state.next, #button{id=State#feed_state.next,
            disabled = element(#iterator.prev, NewLast) == undefined,
            class=[btn, case element(#iterator.prev, NewLast) of undefined -> "disabled"; _ -> "" end],
            body=[#i{class=["icon-chevron-right"]}], data_fields=?TOOLTIP, title= <<"next">>,
            postback={traverse, #iterator.prev, NewLast, State}, delegate=feed_ui}) end,

    wf:update(S#feed_state.page_label, [
        if Total == 0 -> #small{class=["text-warning"],body= <<"[no entries]">>};
        true-> [integer_to_list(NewStart), "-", integer_to_list(NewStart+Current-1), " of ", integer_to_list(Total)] end
    ]),

    if State#feed_state.enable_selection == true ->
        wf:update(S#feed_state.selectall_ctl,
        #checkbox{id=State#feed_state.select_all, class=[checkbox, inline],
            postback={select, State#feed_state.select_all, State}, 
            delegate=S#feed_state.delegate_sel,
            source=[State#feed_state.select_all],
            value = string:join([wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))) || E <- Entries], "|"),
            style=if Total > 0 -> [] ; true-> "display:none;" end}); true -> [] end,
    wf:replace(State#feed_state.delete_btn,
        #link{id=S#feed_state.delete_btn, class=[btn], body=[#i{class=["icon-trash"]}],
            data_fields=?TOOLTIP, title= <<"delete">>,
            postback={delete, State}, delegate=feed_ui}),
    wf:wire("Holder.run();").

traverse_entries(_,undefined,_, #feed_state{more_toolbar=BtnId}) -> self() ! {delivery, [somepath, no_more], [BtnId]}, [];
traverse_entries(_,_,0,_) -> [];
traverse_entries(RecordName, Next, Count, S)->
    case kvs:get(RecordName, Next) of {error, not_found} -> [];
    {ok, R}-> self() ! {delivery, [somepath, show_entry], [R, S]}, [R | traverse_entries(RecordName, element(#iterator.prev, R), Count-1, S)] end.

process_delivery([comment, {_,{_,Fid}}, added], [{error,E}]) -> update_input(Fid,E,"alert-danger");
process_delivery([comment, _, added], [#comment{feed_id=Fid}=C]) ->
    update_entry(prepend, Fid, C),
    case input_state(Fid) of #input_state{}=S ->
        input:event({flush_input, S}); _ -> ok end;

process_delivery([entry, {_,Fid}, added], [{error,E}])  -> update_input(Fid, E, "alert-danger");
process_delivery([entry, {_,Fid}, added], [#entry{}=E]) ->
    update_entry(prepend, Fid, E),
    case input_state(Fid) of #input_state{}=S ->
        input:event({flush_input, S}); _ -> ok end;
process_delivery([Type,_, added], [{error,E}]) -> update_input(?FEED(Type), E, "alert-danger");
process_delivery([Type,_, added], [O]) ->
    update_entry(prepend, ?FEED(Type), O),
    case input_state(?FEED(Type)) of #input_state{}=S -> 
        input:event({flush_input, S}); _ -> ok end;

process_delivery([product,_,updated], [{error,E}]) -> wf:info("[feed_ui] update ~p", [E]);
process_delivery([product,_,updated], [#product{}=P])-> update_entry(replaceWith, ?FEED(product), P);

process_delivery([entry, {_,Fid}, updated], [{error,E}]) -> update_input(Fid, E, "alert-danger");
process_delivery([entry, {_,Fid}, updated], [#entry{}=E])->
    update_entry(replaceWith, Fid, E),
    case input_state(Fid) of #input_state{}=S ->
        input:event({flush_input, S}); _ -> ok end,
    update_input(Fid, "updated", "alert-success");

process_delivery([_,{_,Fid}, deleted], [{error,E}]) -> update_entry(error, Fid, {error,E});
process_delivery([Type,_,    deleted], [{error,E}]) -> update_entry(error, ?FEED(Type), {error,E});
process_delivery([_,{_,Fid}, deleted], [#entry{}=E])-> update_entry(remove, Fid, E);
process_delivery([Type,_,    deleted], [E]) ->         update_entry(remove, ?FEED(Type), E);

process_delivery([show_entry], [Entry, #feed_state{} = S]) ->
    wf:insert_bottom(S#feed_state.entries, #feed_entry{entry=Entry, state=S#feed_state{js_escape=true}}),
    wf:wire("Holder.run();"),
    wf:update(S#feed_state.more_toolbar, #link{ class=[btn,"btn-info"],
                                                body= <<"more">>,
                                                delegate=feed_ui,
                                                postback={check_more, Entry, S}});

process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;
process_delivery(_,_) -> ok.

update_entry(_, Fid, {error, E}) ->
    case feed_state(Fid) of #feed_state{}=S ->
        wf:update(S#feed_state.alert, alert(E, "alert-danger")); _-> skip end;
update_entry(Act, Fid, E) ->
    case feed_state(Fid) of #feed_state{visible_key=Vkey, entry_id=Eid}=S ->
        Id = element(Eid, E),
        UiId = wf:to_list(erlang:phash2(Id)),
        wf:info("[feed_ui] entry ~p ~p [~p] in ~p [~p]", [wf:to_list(Act), Id, UiId, Fid, ?CTX#context.module]),

        if S#feed_state.enable_traverse -> 
            traverse(#iterator.next,E,S);
        true ->
            ?UPDATE_DOM(Act,
                case Act of prepend -> S#feed_state.entries;_-> ?EN_ROW(UiId) end,
                case Act of remove -> [];_-> #feed_entry{entry=E, state=S#feed_state{js_escape=true}} end,
                case Act of remove -> "~s"; _-> "'~s'" end) end,

        case Act of
            remove -> wf:cache(Vkey, lists:delete(element(Eid,E), wf:cache(Vkey)));
            prepend-> wf:cache(Vkey, [element(S#feed_state.entry_id, E) | wf:cache(Vkey)]);
            _ -> ok end,
        wf:cache(medias, undefined),
        deselect(S),
        wf:wire("Holder.run();"); _-> skip end.

update_input(Fid, E, Class) ->
    case input_state(Fid) of #input_state{}=Is ->
        wf:info("[feed_ui] update input ~p for ~p", [Fid, ?CTX#context.module]),
        wf:update(Is#input_state.alert_id, alert(E, Class)); _-> ok end.

to_date(undefined) -> to_date(now());
to_date(Date)->
    {{Y, M, D}, {H,Mi,_}} = calendar:now_to_datetime(Date),
    io_lib:format("~s ~p, ~p at ~p:~p", [?MONTH(M), D, Y, H,Mi]).

alert(Msg, Class) ->
    #panel{class=[alert, "alert-block", fade, in, Class], body=[
        #link{class=[close], url="#", data_fields=[{<<"data-dismiss">>,<<"alert">>}],body= <<"&times;">>},
        #panel{body= Msg}]}.
