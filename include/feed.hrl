%
% Feed specification
%
-record(feed_state, {
    entry_type      = entry,
    entry_id        = #iterator.id,
    container       = feed,
    container_id,
    selectall_ctl   = wf:temp_id(),
    select_all      = wf:temp_id(),
    delete_btn      = wf:temp_id(),
    prev            = wf:temp_id(),
    next            = wf:temp_id(),
    entries         = wf:temp_id(),
    page_label      = wf:temp_id(),
    select_toolbar  = wf:temp_id(),
    feed_toolbar    = wf:temp_id(),
    more_toolbar    = wf:temp_id(),
    close           = wf:temp_id(),
    alert           = wf:temp_id(),
    start           = 0,
    total           = 0,
    current         = 0,
    page_size = 4,
    selected_key = wf:temp_id(),
    visible_key  = wf:temp_id()
    }).

% Feed id based identifies
-define(FD_ID(Id), wf:to_list(erlang:phash2(Id))).
-define(FD_INPUT(Id),     ?FD_ID(Id)++"in").
-define(FD_SELALLCTL(Id), ?FD_ID(Id)++"sallctl").
-define(FD_SELALL(Id),    ?FD_ID(Id)++"all").
-define(FD_PG_LBL(Id),    ?FD_ID(Id)++"pgl").
-define(FD_DEL(Id),       ?FD_ID(Id)++"del").
-define(FD_PREV(Id),      ?FD_ID(Id)++"prev").
-define(FD_NEXT(Id),      ?FD_ID(Id)++"next").
-define(FD_ENTRS(Id),     ?FD_ID(Id)++"es").
-define(FD_MORE(Id),      ?FD_ID(Id)++"mt").
-define(FD_SELTB(Id),     ?FD_ID(Id)++"stb").
-define(FD_TOOLBAR(Id),   ?FD_ID(Id)++"ftb").
-define(FD_SELKEY(Id),    ?FD_ID(Id)++"sk").
-define(FD_VISKEY(Id),    ?FD_ID(Id)++"vk").
-define(FD_CLOSE(Id),     ?FD_ID(Id)++"cs").
-define(FD_EXTHEAD(Id),   ?FD_ID(Id)++"xhd").
-define(FD_STATE(Id), #feed_state{
        container_id    = Id,
        selectall_ctl   = ?FD_SELALLCTL(Id),
        select_all      = ?FD_SELALL(Id),
        delete_btn      = ?FD_DEL(Id),
        prev            = ?FD_PREV(Id),
        next            = ?FD_NEXT(Id),
        entries         = ?FD_ENTRS(Id),
        page_label      = ?FD_PG_LBL(Id),
        select_toolbar  = ?FD_SELTB(Id),
        more_toolbar    = ?FD_MORE(Id),
        feed_toolbar    = ?FD_TOOLBAR(Id),
        selected_key    = ?FD_SELKEY(Id),
        visible_key     = ?FD_VISKEY(Id),
        close           = ?FD_CLOSE(Id)}).
-define(FD_STATE(Id, S), S#feed_state{
        container_id    = Id,
        selectall_ctl   = ?FD_SELALLCTL(Id),
        select_all      = ?FD_SELALL(Id),
        delete_btn      = ?FD_DEL(Id),
        prev            = ?FD_PREV(Id),
        next            = ?FD_NEXT(Id),
        entries         = ?FD_ENTRS(Id),
        page_label      = ?FD_PG_LBL(Id),
        select_toolbar  = ?FD_SELTB(Id),
        more_toolbar    = ?FD_MORE(Id),
        feed_toolbar    = ?FD_TOOLBAR(Id),
        selected_key    = ?FD_SELKEY(Id),
        visible_key     = ?FD_VISKEY(Id),
        close           = ?FD_CLOSE(Id)}).

% Feed entry id based identifiers
-define(EN_ROW(Id),     wf:to_list(Id)++"row").
-define(EN_SEL(Id),     wf:to_list(Id)++"sel").
-define(EN_FROMSEL(Sel),lists:sublist(Sel,1, length(Sel) - length("sel"))).
-define(EN_MEDIA(Id),   wf:to_list(Id)++"media").
-define(EN_TITLE(Id),   wf:to_list(Id)++"t").
-define(EN_DESC(Id),    wf:to_list(Id)++"d").
-define(EN_TOOL(Id),    wf:to_list(Id)++"tb").
-define(EN_CM_COUNT(Id),wf:to_list(Id)++"cc").
