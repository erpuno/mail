defmodule CHAT.Application do
  use Application

  def start(_, _) do
    start_cowboy()
    :kvx.join()
    :syn.init()
    Supervisor.start_link([], strategy: :one_for_one, name: CHAT.Supervisor)
  end

  def start_cowboy() do
    options = [
      port: Application.get_env(:n2o, :port, 8042),
      certfile: :code.priv_dir(:chat) ++ '/ssl/fullchain.pem',
      keyfile: :code.priv_dir(:chat) ++ '/ssl/privkey.pem',
      cacertfile: :code.priv_dir(:chat) ++ '/ssl/fullchain.pem'
    ]

    :cowboy.start_tls(:http, options, %{env: %{dispatch: :n2o_cowboy2.points()}})
  end
end
