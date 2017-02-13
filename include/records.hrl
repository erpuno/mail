%
% deprecated 
%

-define(MAIN_CH,    wf:config(feeds, main_channel, main_ch)).
-define(USR_CHUNK,  wf:config(feeds, usr_chunk, [feed, direct, sent, archive, products, starred, pinned, comments, cart, wishlist])).
-define(GRP_CHUNK,  wf:config(feeds, grp_chunk, [feed, products, comments])).
-define(PRD_CHUNK,  wf:config(feeds, prd_chunk, [features, specs, gallery, videos, reviews, news, bundles, comments])).
