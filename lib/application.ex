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

  def env(_app) do
    [
      {:port, :application.get_env(:n2o, :port, 8002)}
    ]
  end

  def points() do
    :cowboy_router.compile([
      {:_,
       [
         {'/ws/[...]', :n2o_cowboy, []},
         {'/bin/[...]', :cowboy_static, {:dir, "priv/storage", []}},
         {'/app/[...]', :cowboy_static, {:dir, "priv/static", []}}
       ]}
    ])
  end

  def initialize() do
    :cowboy.start_clear(:http, env(:chat), %{env: %{dispatch: points()}})
    :kvs.join
    :n2o.start_ws
  end
end
