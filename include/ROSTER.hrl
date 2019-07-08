-ifndef(_ROSTER_HRL_).
-define(_ROSTER_HRL_, true).
-record('N2O', { tok       = <<>> :: binary()  }).
-record('MUC', { dst       = <<>> :: binary()  }).
-record('P2P', { dst       = <<>> :: binary() | string() }).
-record('Adr', { src       = <<>> :: binary() | string(), dst = []   :: [] | {atom(), #'P2P'{} | #'MUC'{}} }).
-record('Sub', { key       = <<>> :: binary(), adr = []   :: [] | #'Adr'{} }).
-record('Pub', { key       = <<>> :: binary(), adr = []   :: [] | #'Adr'{},
                 tag       = <<>> :: binary(), bin = <<>> :: binary() }).
-record('Ack', { lex       = <<>> :: binary() }).
-record('Nak', { key       = <<>> :: binary() }).
-record('Cut', { id        = <<>> :: binary()  }).
-record('FTP', { id        = []   :: term(),
                 sid       = []   :: term(),
                 filename  = []   :: term(),
                 meta      = []   :: term(),
                 size      = []   :: term(),
                 offset    = []   :: term(),
                 block     = []   :: term(),
                 data      = []   :: term(),
                 status    = []   :: term() }).
-endif.
