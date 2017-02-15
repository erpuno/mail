-module(feed_table).
-compile(export_all).

-include_lib("kvs/include/kvs.hrl").
-include_lib("n2o/include/wf.hrl").
-inlude_lib("nitro/include/nitro.hrl").
-include("elements.hrl"). % nitro here
-include("feed.hrl").

% Render feed as table

render_element(#feed_table{state=S}=F) ->
  Title = case F#feed_table.title of undefined -> ""; T2 when is_atom(T2)-> atom_to_list(T2); T2 -> T2 end,

  Entries = case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_} -> [];
    {ok,Feed} ->
      kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size) end,

  Body = [#feed_entry{module=feed_table, entry=E, state=S} || E <- Entries],
  More = #button{body= <<"more">>, postback = {check_more, <<"last">>, S}},

  wf:render(#table{
    caption = Title,
    header  = F#feed_table.header,
    body    = #tbody{id=S#feed_state.entries, body=Body},
    footer  = [#tr{cells=[#td{body=[More]}]}]
  });

render_element(#feed_entry{entry=E, state=#feed_state{}=S})->
  Id = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
  SelId = ?EN_SEL(Id),

  Checkbox = #td{body=[#checkbox{id=SelId, postback={select, SelId, S},source=[SelId],value=Id}]},
  Entry    = #td{body=wf:to_list(element(S#feed_state.entry_id, E))},

  wf:render(#tr{id=?EN_ROW(Id), cells=[Checkbox,Entry]}).
