defmodule CHAT.Application do
  use Application
  use N2O

  def start(_, _) do
    start_cowboy()
    :kvx.join()
    :syn.init()
    x = Supervisor.start_link([], strategy: :one_for_one, name: CHAT.Supervisor)

    for {{_, _}, pos} <- :lists.zip(:n2o.ring(), :lists.seq(1, length(:n2o.ring()))) do
      :n2o_pi.start(
        pi(module: :n2o_wsnode, table: :ring, sup: CHAT.Supervisor, state: [], name: {:server, pos})
      )
    end

    x
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
