-module(msg).
-compile(export_all).
-include("records.hrl").

notify(EventPath, Data) -> gproc:send({p,l,?MAIN_CH}, {delivery, EventPath, Data}).

