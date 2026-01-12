defmodule MAIL.BanditAdapter do
  use Plug.Router
  require N2O

  @moduledoc """
  WebSocket Adapter for Bandit
  """

  plug :match
  plug :dispatch
  get "/:mod", do: conn |> WebSockAdapter.upgrade(__MODULE__, [module: MAIL.Application.route(mod)], timeout: 60_000) |> halt()

  def init(args), do: {:ok, N2O.cx(module: Keyword.get(args, :module)) }

  def handle_in({"PING", _}, state),                          do: {:reply, :ok, {:text, "PONG"}, state}
  def handle_in({"N2O," <> _ = message, _}, state),           do: response(MAIL.TEXT.info({:text,message},[],state))
  def handle_in({message, _}, state) when is_binary(message), do: response(MAIL.TEXT.info({:text,message},[],state))
  def handle_info(message, state),                            do: response(MAIL.TEXT.info({:text,message},[],state))

  def response({:reply,{:binary,rep},_,s}), do: {:reply,:ok,{:binary,rep},s}
  def response({:reply,{:text,rep},_,s}),   do: {:reply,:ok,{:text,rep},s}
  def response({:reply,{:bert,rep},_,s}),   do: {:reply,:ok,{:binary,:n2o_bert.encode(rep)},s}
  def response({:reply,{:json,rep},_,s}),   do: {:reply,:ok,{:binary,:n2o_json.encode(rep)},s}

  match _ do send_resp(conn, 404, "Please refer to https://mail.n2o.dev for more information.") end
end
