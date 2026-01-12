import Config

config :n2o,
  port: 8043,
  pickler: :n2o_secret,
  mq: :n2o_syn,
  protocols: [:n2o_heart, MAIL.TXT],
  routes: MAIL.Application

config :kvs,
  dba: :kvs_mnesia,
  dba_st: :kvs_stream,
  schema: [:kvs, :kvs_stream, MAIL]
