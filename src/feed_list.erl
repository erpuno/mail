-module(feed_list).
-compile(export_all).

-include_lib("kvs/include/kvs.hrl").
-include_lib("n2o/include/wf.hrl").
-include("elements.hrl").
-include("feed.hrl").

% Render feed as list

render_element(#feed_list{state=S}=F) -> 
    Title = case F#feed_list.title of undefined -> ""; T when is_atom(T)-> atom_to_list(T); T -> T end,

    wf:render(#section{class=[F#feed_list.class], body=[
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,E1r}->
          wf:info(?MODULE,"Container failed: ~p | ~p id:~p", [E1r, S#feed_state.container,  S#feed_state.container_id]),
            #panel{id=S#feed_state.feed_title, class=["fd-title"], body=[
                #panel{class=["fd-title-row"], body=[Title, #span{class=["fd-label"], body= <<" [no feed]">>}]}]};
        {ok,Feed} ->
            Total = element(#container.count, Feed),
            Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),
            Current = length(Entries),
            wf:info(?MODULE, "We gona render: ~p", [Feed]),
            {Last, First} = case Entries of [] -> {#iterator{},#iterator{}}; Es -> {lists:last(Es), lists:nth(1,Es)} end,
            State = S#feed_state{
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

            Body = [F#feed_list.header, #list{id=S#feed_state.entries,  class=["fd-body"], body=UiEntries}],

            More = #panel{id=S#feed_state.more_toolbar, class=["fd-more-toolbar"], body=[
                    if Current < S#feed_state.page_size -> []; true ->
                        #button{class=["fd-btn-more"],
                            body= <<"more">>,
                            delegate=feed_list,
                            postback = {check_more, Last, State}} end]},

            SelectAll = feed_ui:selectall_checkbox(State, Hashs, DisplayStyle),

            TraverseCtl = #span{id=S#feed_state.feed_toolbar,
                      class=["fd-traverse-ctl"],
                      body=if Total > 0 -> [
                    #small{id=S#feed_state.page_label, body=[
                        integer_to_list(State#feed_state.start),"-",integer_to_list(Current)," of ",integer_to_list(Total)]},
                    feed_ui:prev_btn(State, element(#iterator.next, First) /= undefined, First),
                    feed_ui:next_btn(State, element(#iterator.prev, Last) /= undefined, Last)];true -> [] end},
                

            SelectionCtl = 
                #span{id=S#feed_state.select_toolbar,
                    style="display:none;",
                    class=["fd-sel-toolbar"], body=[
                    feed_ui:delete_btn(State),
                    #button{id=S#feed_state.close, class=["fd-close"],
                        postback={cancel_select, State},
                        delegate=feed_list, body= <<"&times;">>}]},
            
            Alert = #span{id=S#feed_state.alert, class=["fd-alert"]},
            FdTitle = #panel{id=S#feed_state.feed_title, class=["fd-title"], body=[
                    SelectAll,
                    #panel{class=["fd-title-i"], body= [Title]},
                    #panel{class=["fd-title-row"], body=[Alert, SelectionCtl, TraverseCtl]}]},

            [FdTitle, Body, More] end]}).
