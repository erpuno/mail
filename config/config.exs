use Mix.Config

config :n2o,
  port: 8043,
  proto: MAIL.Server,
  ws_services: [:chat,:crm],
  mqtt_services: [],
  pickler: :n2o_secret,
  mq: :n2o_syn,
  upload: "./priv/static",
  protocols: [:n2o_heart, MAIL.TXT]

config :kvs,
  dba: :kvs_mnesia,
  dba_st: :kvs_stream,
  schema: [:kvs, :kvs_stream]
