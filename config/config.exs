use Mix.Config

config :n2o,
  pickler: :n2o_secret,
  mq: :n2o_syn,
  upload: "./priv/static",
  protocols: [:n2o_heart, CHAT.BER, CHAT.TXT]

config :kvx,
  dba: :kvx_rocks,
  dba_st: :kvx_st,
  schema: [:kvx, :kvx_stream]
