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
-define(THUMB_SIZE,     [{139, 80}, {270, 124}, {180,180}, {200, 200}, {570, 570}, {716, 480}, {1170, 350}]).
-define(MONTH(M),       element(M, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"})).

-record(input, {?ELEMENT_BASE(input), state, icon="icon-edit"}).

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

    post_btn    = <<"post">>,
    close_btn   = <<"close">>,
    update_btn  = <<"update">>,
    cancel_btn  = <<"cancel">>,
    expand_btn  = <<"">>,
    expand_class= [btn, "btn-info"],
    post_class  = [btn, "btn-info"],
    close_class = [btn],
    cancel_class= [btn] }).
