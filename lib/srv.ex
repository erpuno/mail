defmodule CHAT.Server do
  use N2O, with: [:n2o, :kvx]
  require CHAT

  def info(CHAT."Cut"(id: id), r, cx(session: from) = s) do
    KVX.cut(from, id)
    {:reply, {:default, CHAT."Ack"(lex: id)}, r, s}
  end

  def info(CHAT."Pub"(key: id, adr: CHAT."Adr"(dst: {:p2p, CHAT."P2P"(dst: to)})) = msg, r, s) do
    KVX.append(msg, to)
    N2O.send({:client, to}, {:flush, msg})
    {:reply, {:binary, CHAT."Ack"(lex: id)}, r, s}
  end

  def info(msg, r, s), do: {:unknown, msg, r, s}
end
