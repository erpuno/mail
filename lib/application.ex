defmodule MAIL.Application do
  @moduledoc """
  `MAIL.Application` is an
  `Erlang/OTP` application powered by N2O messaging protocol.
  It is implemented as a ring of protocol nodes.
  """
  use Application
  use N2O

  def start(_, _) do
      :kvs.join
      :n2o.start_ws
      :supervisor.start_link({:local,MAIL},MAIL,[])
  end

end
