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
    {{:ws, 4}, #PID<0.258.0>, :worker, [:n2o_wsnode]},
    {{:ws, 3}, #PID<0.257.0>, :worker, [:n2o_wsnode]},
    {{:ws, 2}, #PID<0.256.0>, :worker, [:n2o_wsnode]},
    {{:ws, 1}, #PID<0.255.0>, :worker, [:n2o_wsnode]},
    {{:caching, 'timer'}, #PID<0.201.0>, :worker, [:n2o]}
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
    :cowboy.start_tls(:http, :n2o_cowboy.env(:chat), %{env: %{dispatch: :n2o_cowboy2.points()}})
    :kvx.join()
    :syn.init()
    :n2o.start_ws_ring()
  end
end
