
-type route() :: [term()]. %% list representation of the route. For example
                           %% ['X', "Y", 1] will be transformed to <<"X.Y.1">>

-define(DEAD_LETTER_EXCHANGE, <<"nsm_bg.dead_letter_exchange.fanout">>).
-define(BOOTSTRAP_WORKER_QUEUE, <<"nsm_bg.worker.bootstrap2">>).
-define(REANIMATOR_QUEUE_NAME(Node),list_to_binary(lists:concat(["nsm_bg.reanimator.", Node]))).
-define(FEED_WORKER_NAME(Type, Name), [feed, worker, Type, Name]).

-define(MAX_BAD_OPERATION, 5).
-define(SMTP_USER, nsm_db:get_config("smtp/user",     "noreply@kakaranet.com")).
-define(SMTP_PASSWD, nsm_db:get_config("smtp/password", "unknown")).
-define(SMTP_HOST, nsm_db:get_config("smtp/host",     "smtp.kakaranet.com")).
-define(SMTP_PORT, nsm_db:get_config("smtp/port",     587)).
-define(SMTP_SSL, nsm_db:get_config("smtp/with_ssl", false)).
