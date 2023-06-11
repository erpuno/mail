defmodule MAIL.TXT do
  @moduledoc """
  `MAIL.TXT` is a WebSocket client interface and client
  socket protocol (session level) representation and
  textual protocol termination.
  """
  use N2O, with: [:n2o, :kvs]
  require MAIL

  defp format_msg(
         MAIL."Pub"(bin: pl, key: id, adr: MAIL."Adr"(src: fr, dst: {:p2p, MAIL."P2P"(dst: to)}))
       ) do
    :io_lib.format('~s:~s:~s:~s', [fr, to, id, pl])
  end

  @doc """
  N2O protocol implementation (client, session part).
  """
  def info({:text, <<"AUTH", x::binary>>}, r, s) do
    a = :string.trim(:erlang.binary_to_list(x))
    key = '/mail/' ++ a
    N2O.reg({:client, key})
    KVS.ensure(writer(id: key))

    {:reply, {:text, <<"USER " <> :erlang.list_to_binary(a)::binary>>}, r, cx(s, session: a)}
  end

  def info({:text, <<"SEND", _::binary>>}, r, cx(session: []) = s),
    do: {:reply, {:text, "Please login with AUTH. Try HELP."}, r, s}

  def info({:text, <<"SEND", x::binary>>}, r, cx(session: from) = s) do
    case :string.tokens(:string.trim(:erlang.binary_to_list(x)), ' ') do
      [to | rest] ->
        key = KVS.seq([], [])

        msg =
          MAIL."Pub"(
            key: key,
            adr: MAIL."Adr"(src: from, dst: {:p2p, MAIL."P2P"(dst: to)}),
            bin: :erlang.iolist_to_binary(:string.join(rest, ' '))
          )

        res =
          case MAIL.user(to) do
            false -> "ERROR user doesn't exist."
            true -> :n2o_ring.send(:ws, :mail, {:publish, self(), from, msg})
              <<>>
          end

        {:reply, {:text, res}, r, s}

      _ ->
        {:reply, {:text, "ERROR in request."}, r, s}
    end
  end

  def info({:text, <<"BOX">>}, r, cx(session: from) = s) do
    KVS.ensure(writer(id: '/mail/' ++ from))
    fetch = reader(KVS.take(reader(:kvs.reader('/mail/'++from), args: -1)), :args)

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
    do: {:reply, {:text, <<"AUTH <user>\n| SEND <user> <msg>\n| BOX\n| CUT <id>.">>}, r, s}

  def info({:text, <<"CUT", x::binary>>}, r, cx(session: from) = s) do
    case :string.tokens(:string.trim(:erlang.binary_to_list(x)), ' ') do
      [id] ->
        case KVS.cut('/mail/' ++ from, id) do
          {:ok, count} -> {:reply, {:text, <<"ERASED ", MAIL.bin(count)::binary>>}, r, s}
          {:error, _} -> {:reply, {:text, <<"NOT FOUND ">>}, r, s}
        end

      _ ->
        {:reply, {:text, <<"ERROR in request.">>}, r, s}
    end
  end

  def info({:forward, MAIL."Pub"() = m}, r, s),
    do: {:reply, {:text, "NOTIFY " <> :erlang.list_to_binary(format_msg(m))}, r, s}

  def info({:forward, text}, r, s), do: {:reply, {:text, text}, r, s}
  def info({:text, _}, r, s), do: {:reply, {:text, "Try HELP"}, r, s}
  def info(msg, r, s), do: {:unknown, msg, r, s}
end
