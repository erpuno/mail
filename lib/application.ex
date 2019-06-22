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

  ```erlang
  iex(1)> Supervisor.which_children(:n2o)
  [
    {{:ring, {:server, 4}}, #PID<0.257.0>, :worker, [:n2o_wsnode]},
    {{:ring, {:server, 3}}, #PID<0.256.0>, :worker, [:n2o_wsnode]},
    {{:ring, {:server, 2}}, #PID<0.255.0>, :worker, [:n2o_wsnode]},
    {{:ring, {:server, 1}}, #PID<0.254.0>, :worker, [:n2o_wsnode]},
    {{:ring, 4}, #PID<0.200.0>, :worker, [:n2o_vnode]},
    {{:ring, 3}, #PID<0.199.0>, :worker, [:n2o_vnode]},
    {{:ring, 2}, #PID<0.198.0>, :worker, [:n2o_vnode]},
    {{:ring, 1}, #PID<0.197.0>, :worker, [:n2o_vnode]},
    {{:caching, 'timer'}, #PID<0.196.0>, :worker, [:n2o]}
  ]
  ```
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
        pi(module: :n2o_wsnode, table: :ring, sup: :n2o, state: [], name: {:server, pos})
      )
    end
  end
end
