defmodule MAIL.Application do

  use Application
  require N2O

  def route(<<"bert",_::binary>>), do: MAIL.BERT
  def route(<<_::binary>>),        do: MAIL.TEXT

  def finish(state, ctx),  do: {:ok, state, ctx}
  def init(state, context) do
      %{path: path} = N2O.cx(context, :req)
      {:ok, state, N2O.cx(context, path: path, module: route(path))}
  end

  def start(_, _) do
      :kvs.join
      children = [ { Bandit, scheme: :http, port: 8043, plug: MAIL.BanditAdapter } ]
      Supervisor.start_link(children, strategy: :one_for_one, name: MAIL.Supervisor)
  end

end
