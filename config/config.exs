use Mix.Config

config :n2o,
  port: 8042,
  proto: CHAT.Server,
  ws_services: ['chat','crm'],
  mqtt_services: [],
  pickler: :n2o_secret,
  mq: :n2o_syn,
  upload: "./priv/static",
  protocols: [:n2o_heart, CHAT.TXT]

config :kvs,
  dba: :kvs_rocks,
  dba_st: :kvs_st,
  schema: [:kvs, :kvs_stream, CHAT]
