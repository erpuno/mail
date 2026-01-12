defmodule MAIL.TEXT do
  require MAIL
  require KVS
  require N2O

  @moduledoc """
  MAIL Text Protocol for WebSocket
  """

  def format_msg(MAIL."Pub"(key: key, adr: adr, tag: tag, bin: bin)) do
      :io_lib.format(~c"~s:~s:~s:~s", [key, adr, tag, bin])
  end

  def info({:text, <<"AUTH", x::binary>>}, r, s) do
      a = :string.trim(:erlang.binary_to_list(x))
      key = ~c"/mail/" ++ a
      :n2o.reg({:client, key})
      :kvs.ensure(KVS.writer(id: key))
      {:reply, {:text, <<"USER " <> :erlang.list_to_binary(a)::binary>>}, r, N2O.cx(s, session: a)}
  end

  def info({:text, <<"SEND", _::binary>>}, r, N2O.cx(session: []) = s) do
      {:reply, {:text, "Please login with AUTH. Try HELP."}, r, s}
  end

  def info({:text, <<"SEND", x::binary>>}, r, N2O.cx(session: from) = s) do
      case :string.tokens(:string.trim(:erlang.binary_to_list(x)), ~c" ") do
        [to | rest] ->
             key = :kvs.seq([], [])
             msg = MAIL."Pub"(key: key, adr: MAIL."Adr"(src: from, dst: {:p2p, MAIL."P2P"(dst: to)}),
                            bin: :erlang.iolist_to_binary(:string.join(rest, ~c" ")))
             res = case MAIL.user(to) do
               false -> "ERROR user doesn't exist."
               true -> MAIL.BERT.info(msg, r, s) ; <<>>
             end
             {:reply, {:text, res}, r, s}
        _ -> {:reply, {:text, "ERROR in request."}, r, s}
      end
  end

  def info({:text, <<"BOX">>}, r, N2O.cx(session: from) = s) do
      :kvs.ensure(:kvs.writer(id: ~c"/mail/" ++ from))
      fetch = KVS.reader(:kvs.take(KVS.reader(:kvs.reader(~c"/mail/"++from), args: -1)), :args)
      res = "LIST\n" <> :erlang.list_to_binary(:string.join(for m <- :lists.reverse(fetch) do format_msg(m) end, ~c"\n"))
      {:reply, {:text, res}, r, s}
  end

  def info({:text, "HELP"}, r, s) do
      {:reply, {:text, <<"AUTH <user>\n| SEND <user> <msg>\n| BOX\n| CUT <id>.">>}, r, s}
  end

  def info({:text, <<"CUT", x::binary>>}, r, N2O.cx(session: from) = s) do
      case :string.tokens(:string.trim(:erlang.binary_to_list(x)), ~c" ") do
        [id] ->
           case :kvs.cut(~c"/mail/" ++ from, id) do
             {:ok, count} -> {:reply, {:text, <<"ERASED ", MAIL.bin(count)::binary>>}, r, s}
             {:error, _} -> {:reply, {:text, <<"NOT FOUND ">>}, r, s}
           end
        _ -> {:reply, {:text, <<"ERROR in request.">>}, r, s}
      end
  end

  def info({:forward, MAIL."Pub"() = m}, r, s) do
      {:reply, {:text, "NOTIFY " <> :erlang.list_to_binary(format_msg(m))}, r, s}
  end

  def info({:forward, text}, r, s), do: {:reply, {:text, text}, r, s}
  def info({:text, _}, r, s), do: {:reply, {:text, "Try HELP"}, r, s}
  def info(msg, r, s), do: {:unknown, msg, r, s}

end
