use Mix.Config

config :n2o,
  proto: CHAT.Server,
  pickler: :n2o_secret,
  tables: [ :cookies, :file, :caching, :ring, :part, :async ],
  mq: :n2o_syn,
  upload: "./priv/static",
  protocols: [:n2o_heart, CHAT.TXT]

config :kvx,
  dba: :kvx_rocks,
  dba_st: :kvx_st,
  schema: [:kvx, :kvx_stream, CHAT]
