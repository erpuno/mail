-include_lib("nitro/include/nitro.hrl").

-define(FEED_UI(Module),  {?ELEMENT_BASE(Module), state, header=[]}).

-record(feed_ui,          ?FEED_UI(feed_ui)). % should be feed_panel/feed_div/feed_block
-record(feed_table,       ?FEED_UI(feed_table)).
-record(feed_list,        ?FEED_UI(feed_list)).

-record(feed_entry,     {?ELEMENT_BASE(feed_ui), entry, state}).
-record(entry_media,    {?ELEMENT_BASE(feed_ui), media, mode}).
