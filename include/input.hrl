-ifndef(TEMP_ID).
-define(TEMP_ID, wf:temp_id()).
-endif.

-ifndef(ROOT).
-define(ROOT, code:priv_dir(web)).
-endif.

-ifndef(DIR).
-define(DIR(Path), "static/"++Path).
-endif.

-define(CURRENCY,       [{<<"Dollar">>, <<"USD">>}]).
-define(THUMB_SIZE,     [{139, 80}, {270, 124}, {200, 200}, {370, 250}, {500, 500}, {1170, 350}]).
-define(MONTH(M),       element(M, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"})).

% from kvs.social
-define(USR_FEED, users).
-define(PRD_FEED, products).
-define(GRP_FEED, groups).
-define(ENT_FEED, entries).
-define(CMT_FEED, comments).
-define(FEED(Type), case Type of 
  user -> ?USR_FEED; 
  product -> ?PRD_FEED; 
  group -> ?GRP_FEED; 
  entry-> ?ENT_FEED; 
  comment-> ?CMT_FEED;
  _-> undefined end).

% social schema
-record(media, {
        id,
        title :: iolist(),
        width,
        height,
        html :: iolist(),
        url :: iolist(),
        version,
        thumbnail_url :: iolist(),
        type :: {atom(), atom() | string()},
        thumbnail_height}).

-record(group_subscription, {
        key,
        who,
        where,
        type,
        posts_count = 0 :: integer() % we need this for sorting and counting is expensive
        }).

-include_lib("nitro/include/nitro.hrl").

-record(feed_input, {?ELEMENT_BASE(input), state, icon="fa fa-edit icon-edit"}).

% fake missing records
-record(textboxlist, {?ELEMENT_BASE(input), placeholder, values}).
-record(htmlbox, {?ELEMENT_BASE(element_textbox), root, html, post_write, delegate_api, img_tool, post_target, size}).
-record(carousel, {?ELEMENT_BASE(panel), caption, indicators, items}).

-record(input_state, {
    id          = ?TEMP_ID,
    form_id     = ?TEMP_ID,
    toolbar_id  = ?TEMP_ID,
    recipients_id = ?TEMP_ID,
    title_id    = ?TEMP_ID,
    body_id     = ?TEMP_ID,
    media_id    = ?TEMP_ID,
    price_id    = ?TEMP_ID,
    currency_id = ?TEMP_ID,
    alert_id    = ?TEMP_ID,
    upload_id   = ?TEMP_ID,
    post_id     = ?TEMP_ID,
    scope_id    = ?TEMP_ID,

    class = [],
    control_title = <<"">>,
    collapsed = false,
    post_collapse=false,
    update = false,
    collect_msg = true,
    entry_id,
    entry_type  = entry,

    show_recipients = true,
    role=group,
    placeholder_rcp = <<"">>,
    recipients = [],

    show_title = true,
    placeholder_ttl = <<"Title">>,
    title,

    show_body  = true,
    description,

    show_price = false,
    price,
    show_scope = false,

    show_upload = false,
    upload_title,
    upload_dir = ?DIR("anonymous"),
    delegate_query = input,
    post_upload = attach_media,
    delegate_api = input,
    img_tool = gm,

    show_media = true,
    single_media = false,
    medias=[],

    delegate    = input,
    post_btn    = <<"post">>,
    close_btn   = <<"close">>,
    update_btn  = <<"update">>,
    cancel_btn  = <<"cancel">>,
    expand_btn  = <<"">>,
    expand_class= [btn, "btn-info"],
    post_class  = [btn, "btn-info"],
    close_class = [btn, "btn-default"],
    cancel_class= [btn, "btn-default"] }).
