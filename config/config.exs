use Mix.Config

config :n2o,
  pickler: :n2o_secret,
  mq: :n2o_syn,
  upload: "./priv/static",
  proto: CHAT.Server,
  protocols: [:n2o_heart, CHAT.TXT]

config :kvx,
  dba: :kvx_rocks,
  dba_st: :kvx_st,
  schema: [:kvx, :kvx_stream, CHAT]
