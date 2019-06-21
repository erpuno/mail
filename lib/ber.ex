defmodule CHAT.BER do
  use N2O, with: [:n2o, :kvx]

  def info({:text, <<"N2O", x::binary>>}, r, s) do
    a = :string.trim(:erlang.binary_to_list(x))
    N2O.reg({:client, a})
    KVX.ensure(writer(id: a))
    "TXT N2O: #{a}~n" |> IO.inspect()

    {:reply, {:text, <<:erlang.list_to_binary("USER " <> a)::binary>>}, r,
     cx(s, session: a)}
  end
end
