defmodule MAIL.BERT do
  require MAIL
  require N2O

  @moduledoc """
  MAIL Binary Protocol
  """

  def info(MAIL."Cut"(id: id), r, N2O.cx(session: from) = s) do
    :kvs.cut(~c"/mail/" ++ from, id)
    {:reply, {:default, MAIL."Ack"(lex: id)}, r, s}
  end

  def info(MAIL."Pub"(key: id, adr: MAIL."Adr"(dst: {:p2p, MAIL."P2P"(dst: to)})) = msg, r, s) do
    key = ~c"/mail/" ++ to
    :kvs.append(msg, key)
    :n2o.send({:client, key}, {:forward, msg})
    {:reply, {:binary, MAIL."Ack"(lex: id)}, r, s}
  end

  def info(msg, r, s), do: {:unknown, msg, r, s}

end
