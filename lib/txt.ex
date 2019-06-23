defmodule CHAT.TXT do
  @moduledoc """
  `CHAT.TXT` is a WebSocket client interface and client
  socket protocol (session level) representation and
  textual protocol termination.
  """
  use N2O, with: [:n2o, :kvx]
  require CHAT

  defp format_msg(
         CHAT."Pub"(bin: pl, key: id, adr: CHAT."Adr"(src: fr, dst: {:p2p, CHAT."P2P"(dst: to)}))
       ) do
    :io_lib.format('~s:~s:~s:~s', [fr, to, id, pl])
  end

  @doc """
  N2O protocol implementation (client, session part).
  """
  def info({:text, <<"N2O", x::binary>>}, r, s) do
    a = :string.trim(:erlang.binary_to_list(x))
    N2O.reg({:client, a})
    KVX.ensure(writer(id: a))

    {:reply, {:text, <<"USER " <> :erlang.list_to_binary(a)::binary>>}, r, cx(s, session: a)}
  end

  def info({:text, <<"SEND", _::binary>>}, r, cx(session: []) = s),
    do: {:reply, {:text, "Please login with N2O. Try HELP."}, r, s}

  def info({:text, <<"SEND", x::binary>>}, r, cx(session: from) = s) do
    case :string.tokens(:string.trim(:erlang.binary_to_list(x)), ' ') do
      [to | rest] ->
        key = KVX.seq([], [])

        msg =
          CHAT."Pub"(
            key: key,
            adr: CHAT."Adr"(src: from, dst: {:p2p, CHAT."P2P"(dst: to)}),
            bin: :erlang.iolist_to_binary(:string.join(rest, ' '))
          )

        res =
          case CHAT.user(to) do
            false ->
              "ERROR user doesn't exist."

            true ->
              {:ring, n} = :n2o_ring.lookup(to)
              N2O.send({:server, n}, {:publish, self(), from, msg})
              <<>>
          end

        {:reply, {:text, res}, r, s}

      _ ->
        {:reply, {:text, "ERROR in request."}, r, s}
    end
  end

  def info({:text, <<"BOX">>}, r, cx(session: from) = s) do
    KVX.ensure(writer(id: from))
    fetch = reader(KVX.take(reader(:kvx.reader(from), args: -1)), :args)

    res =
      "LIST\n" <>
        :erlang.list_to_binary(
          :string.join(
            for m <- :lists.reverse(fetch) do
              format_msg(m)
            end,
            '\n'
          )
        )

    {:reply, {:text, res}, r, s}
  end

  def info({:text, "HELP"}, r, s),
    do: {:reply, {:text, <<"N2O <user>\n| SEND <user> <msg>\n| BOX\n| CUT <id>.">>}, r, s}

  def info({:text, <<"CUT", x::binary>>}, r, cx(session: from) = s) do
    case :string.tokens(:string.trim(:erlang.binary_to_list(x)), ' ') do
      [id] ->
        case KVX.cut(from, id) do
          {:ok, count} -> {:reply, {:text, <<"ERASED ", CHAT.bin(count)::binary>>}, r, s}
          {:error, _} -> {:reply, {:text, <<"NOT FOUND ">>}, r, s}
        end

      _ ->
        {:reply, {:text, <<"ERROR in request.">>}, r, s}
    end
  end

  def info({:flush, CHAT."Pub"() = m}, r, s),
    do: {:reply, {:text, "NOTIFY " <> :erlang.list_to_binary(format_msg(m))}, r, s}

  def info({:flush, text}, r, s), do: {:reply, {:text, text}, r, s}
  def info({:text, _}, r, s), do: {:reply, {:text, "Try HELP"}, r, s}
  def info(msg, r, s), do: {:unknown, msg, r, s}
end
