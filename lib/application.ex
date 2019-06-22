defmodule CHAT.Application do
  @moduledoc """
  `CHAT.Application` is an
  `Erlang/OTP` application powered by N2O messaging protocol.
  It is implemented as a ring of protocol nodes.
  """
  use Application
  use N2O

  def start(_, _) do
    initialize()
    Supervisor.start_link([], strategy: :one_for_one, name: CHAT.Supervisor)
  end

  @doc """
  TLS cert options for `:cowboy`, `:syn` and `:kvx` start. This function
  also spawns ring of `:n2o_wsnode` modules under N2O supervision tree.

  Example:

  ```elixir
  iex(1)> Supervisor.which_children(:n2o)
  [
    {{:part, 4}, #PID<0.300.0>, :worker, [:n2o_wsnode]},
    {{:part, 3}, #PID<0.299.0>, :worker, [:n2o_wsnode]},
    {{:part, 2}, #PID<0.298.0>, :worker, [:n2o_wsnode]},
    {{:part, 1}, #PID<0.297.0>, :worker, [:n2o_wsnode]},
    {{:ring, 4}, #PID<0.243.0>, :worker, [:n2o_vnode]},
    {{:ring, 3}, #PID<0.242.0>, :worker, [:n2o_vnode]},
    {{:ring, 2}, #PID<0.241.0>, :worker, [:n2o_vnode]},
    {{:ring, 1}, #PID<0.240.0>, :worker, [:n2o_vnode]},
    {{:caching, 'timer'}, #PID<0.239.0>, :worker, [:n2o]}
  ]
  ```

  In example you see two rings: `ring` for MQTT workers and `part` for WebSocket workers.

  File `config/config.exs` should contain `proto` N2O parameter, the module
  which contains N2O protocol that will be runned inside ring worker:

  ```elixir
  config :n2o,
    proto: CHAT.Server
  ```

  In CHAT application this `:n2o_wsnode` worker is `CHAT.Server` module.
  """
  def initialize() do
    options = [
      port: Application.get_env(:n2o, :port, 8042),
      certfile: :code.priv_dir(:chat) ++ '/ssl/fullchain.pem',
      keyfile: :code.priv_dir(:chat) ++ '/ssl/privkey.pem',
      cacertfile: :code.priv_dir(:chat) ++ '/ssl/fullchain.pem'
    ]

    :cowboy.start_tls(:http, options, %{env: %{dispatch: :n2o_cowboy2.points()}})
    :kvx.join()
    :syn.init()

    for {{_, _}, pos} <- :lists.zip(:n2o.ring(), :lists.seq(1, length(:n2o.ring()))) do
      :n2o_pi.start(
        pi(module: :n2o_wsnode, table: :part, sup: :n2o, state: [], name: pos)
      )
    end
  end
end
