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

render_element(#feed_ui{state=undefined})-> wf:render(#panel{class=["fd-nostate"], body= <<"no state">>});

render_element(#feed_ui{state=S}=F) ->
    Title = case F#feed_ui.title of undefined -> ""; T when is_atom(T)-> atom_to_list(T); T -> T end,
    Icon = case F#feed_ui.icon of undefined -> []; L when is_list(L) -> L; I -> [I] end,
    IconLink = case F#feed_ui.icon_url of
        undefined when Icon == [] -> [];
        undefined -> #i{class=[Icon]};
        Url-> #link{url=Url, body=[#i{class=[Icon]}], data_fields=?DATA_TAB} end,
    ExtHeader = if S#feed_state.show_header == true -> [F#feed_ui.header]; true -> [] end,

    wf:render(#section{class=["fd-ui", F#feed_ui.class], body=[
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_}->
            #panel{id=S#feed_state.feed_title, class=["fd-title"], body=[
                #panel{class=["fd-title-i"],  body=[#i{class=Icon}]},
                #panel{class=["fd-title-row"], body=[Title, #span{class=["fd-label"], body= <<" [no feed]">>}]}]};
        {ok,Feed} ->
            Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),

            Total = element(#container.entries_count, Feed),
            Current = length(Entries),
            {Last, First} = case Entries of [] -> {#iterator{},#iterator{}}; Es -> {lists:last(Es), lists:nth(1,Es)} end,
            State = S#feed_state{
                start_element = First,
                last_element = Last,
                start = 1,
                total = Total,
                current = Current},
            DisplayStyle = if Total > 0 -> [] ; true-> "display:none;" end,

            {UiEntries, {Ids, Hashs}} = lists:mapfoldl(fun(E,{IdsIn, HashIn})->
                Id = element(S#feed_state.entry_id,E),
                Hash = wf:to_list(erlang:phash2(Id)),
                {#feed_entry{entry=E, state=State}, {[Id|IdsIn],[Hash|HashIn]}} end, {[], []}, Entries),

            wf:cache(S#feed_state.visible_key, Ids),
            wf:cache(S#feed_state.selected_key,[]),

            Body = if S#feed_state.html_tag == table ->
                #table{class=["fd-table"], header=ExtHeader, body=
                    [#tbody{id=S#feed_state.entries, class=["fd-tbody"], body=UiEntries}]};
                true -> [ExtHeader, #panel{id=S#feed_state.entries, class=["fd-body"], body=UiEntries}] end,

            More = if S#feed_state.enable_traverse == false ->
                #panel{id=S#feed_state.more_toolbar, class=["fd-more-toolbar"], body=[
                    if Current < S#feed_state.page_size -> []; true ->
                        #link{class=["fd-btn-more"],
                            body= <<"more">>,
                            delegate=feed_ui,
                            postback = {check_more, Last, State}} end]};true -> [] end,

            SelectAll = selectall_checkbox(State, Hashs, DisplayStyle),

            TraverseCtl = if S#feed_state.enable_traverse == true ->
                #span{id=S#feed_state.feed_toolbar,
                      class=["fd-traverse-ctl"],
                      body=if Total > 0 -> [
                    #small{id=S#feed_state.page_label, body=[
                        integer_to_list(State#feed_state.start),"-",integer_to_list(Current)," of ",integer_to_list(Total)]},
                    prev_btn(State, element(#iterator.next, First) /= undefined, First),
                    next_btn(State, element(#iterator.prev, Last) /= undefined, Last)];
                true-> [#i{id=S#feed_state.prev},#i{id=S#feed_state.next}] end}; true -> [] end,

            SelectionCtl = #span{id=S#feed_state.select_toolbar, style="display:none;",class=["fd-sel-toolbar"], body=[
                delete_btn(State),
                F#feed_ui.selection_ctl,
                #button{id=S#feed_state.close, class=["fd-close"],
                    postback={cancel_select, State},
                    delegate=feed_ui, body= <<"&times;">>}]},
            FdTitle = if S#feed_state.show_title == true ->
                Alert = #span{id=S#feed_state.alert, class=["fd-alert"]},
                #panel{id=S#feed_state.feed_title, class=["fd-title"], body=[
                    SelectAll,
                    #panel{class=["fd-title-i"], body= [IconLink, Title]},
                    #panel{class=["fd-title-row"], body=[Alert, SelectionCtl, TraverseCtl]}]};
            true -> [] end,

            [FdTitle, Body, More] end]});

% feed entry representation

render_element(#feed_entry{entry=E, state=#feed_state{html_tag=table}=S})->
    Id = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    SelId = ?EN_SEL(Id),
    El = element_tr:render_element(#tr{id=?EN_ROW(Id), cells=[
        if S#feed_state.enable_selection -> #td{body=[
            #checkbox{id=SelId,
                postback={select, SelId, S},
                delegate=S#feed_state.delegate_sel,
                source=[SelId], value=Id}]}; true -> [] end,
        #row_entry{entry=E, state=S, module=S#feed_state.delegate}]}),
    if S#feed_state.js_escape -> wf:js_escape(El); true -> El end;
render_element(#feed_entry{entry=E, state=S})->
    Id = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    SelId = ?EN_SEL(Id),
    El = element_panel:render_element(#panel{id=?EN_ROW(Id), class=["fd-entry"], body=[
        if S#feed_state.enable_selection ->
            #panel{class=["fd-entry-sel"], body=#checkbox{id=SelId,
                    postback={select, SelId, S},
                    delegate=S#feed_state.delegate_sel,
                    source=[SelId], value=Id}}; true -> [] end,
        #panel{class=["fd-entry-container"], body=#div_entry{entry=E, state=S, module=S#feed_state.delegate}} ]}),
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

delete_btn(S)->
    #button{id=S#feed_state.delete_btn,
        class=["fd-btn-delete"],
        body=[#i{class=["fd-trash"]}],
        data_fields=?TOOLTIP, title= <<"delete">>,
        postback={delete, S}, delegate=feed_ui}.
next_btn(S, HasPrev,Last)->
    #button{id=S#feed_state.next,
        disabled = not HasPrev,
        class=["fd-btn-prev", if HasPrev -> ""; true -> disabled end],
        body=[#i{class=["fd-chevron-right"]}], title= <<"next">>,
        delegate=feed_ui, data_fields=?TOOLTIP,
        postback={traverse, #iterator.prev, Last, S}}.
prev_btn(S, HasNext, First)->
    #button{id=S#feed_state.prev,
        disabled = not HasNext,
        class=["fd-btn-next", if HasNext -> ""; true -> disabled end],
        body=[#i{class=["fd-chevron-left"]}], data_fields=?TOOLTIP, title= <<"previous">>,
        postback={traverse, #iterator.next, First, S}, delegate=feed_ui}.
selectall_checkbox(S, Hashs, Style)->
    if S#feed_state.enable_selection == true ->
        #checkbox{id=S#feed_state.select_all, class=["fd-checkbox"],
            postback={select, S#feed_state.select_all, S},
            delegate=S#feed_state.delegate_sel,
            source=[S#feed_state.select_all],
            value= string:join(Hashs, "|"),
            style=Style}; true -> [] end.

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
        Prev -> kvs:entries(S#feed_state.entry_type, Prev, S#feed_state.page_size, Direction) end,

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

    {UiEntries, {Ids, Hashs}} = lists:mapfoldl(fun(E,{IdsIn, HashIn})->
        Id = element(S#feed_state.entry_id,E),
        Hash = wf:to_list(erlang:phash2(Id)),
        {#feed_entry{entry=E, state=State#feed_state{js_escape=true}}, {[Id|IdsIn],[Hash|HashIn]}} end, {[], []}, Entries),

    wf:cache(S#feed_state.visible_key, Ids),
    wf:cache(S#feed_state.selected_key,[]),

    wf:update(S#feed_state.entries, UiEntries),

    if Total == 0 ->
        wf:replace(S#feed_state.prev, #i{id=S#feed_state.prev}),
        wf:replace(S#feed_state.next, #i{id=S#feed_state.next});
    true ->
        wf:replace(S#feed_state.prev, prev_btn(State, element(#iterator.next, NewFirst) /= undefined, NewFirst)),
        wf:replace(S#feed_state.next, next_btn(State, element(#iterator.prev, NewLast) /= undefined, NewLast)) end,

    wf:update(S#feed_state.page_label, [
        if Total == 0 -> #small{class=["text-warning"],body= <<"[no entries]">>};
        true-> [integer_to_list(NewStart), "-", integer_to_list(NewStart+Current-1), " of ", integer_to_list(Total)] end
    ]),

    DisplayStyle = if Total > 0 -> [] ; true-> "display:none;" end,
    wf:update(State#feed_state.selectall_ctl, selectall_checkbox(State, Hashs, DisplayStyle)),

    wf:replace(State#feed_state.delete_btn, delete_btn(State)),
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
