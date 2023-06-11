defmodule MAIL.Server do
  @moduledoc """
  The `MAIL.Server` is a ring node implemented as `:n2o_pi`
  worker that handle all the incoming requests though hash function and
  implements RPC over MQ pattern. It suports both SYN and GPROC message buses (QoS=0). 
  """
  use N2O, with: [:n2o, :kvs]
  require MAIL

  @doc """
  N2O protocol implementation (server part).
  """
  def info(MAIL."Cut"(id: id), r, cx(session: from) = s) do
    KVS.cut('/mail/' ++ from, id)
    {:reply, {:default, MAIL."Ack"(lex: id)}, r, s}
  end

  def info(MAIL."Pub"(key: id, adr: MAIL."Adr"(dst: {:p2p, MAIL."P2P"(dst: to)})) = msg, r, s) do
    key = '/mail/' ++ to
    KVS.append(msg, key)
    N2O.send({:client, key}, {:forward, msg})
    {:reply, {:binary, MAIL."Ack"(lex: id)}, r, s}
  end

  def info(msg, r, s), do: {:unknown, msg, r, s}
end
