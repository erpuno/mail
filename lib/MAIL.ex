defmodule MAIL do

  require Record
  require KVS

  Enum.each(Record.extract_all(from_lib: "mail/include/ROSTER.hrl"), fn {name, definition} -> Record.defrecord(name, definition) end)

  def bin(key) do :erlang.list_to_binary(:io_lib.format("~p", [key])) end
  def metainfo() do KVS.schema(name: :mail, tables: [KVS.table(name: :"Pub", fields: [:key, :adr, :tag, :bin])]) end
  def user(id) do case :kvs.get(:writer, ~c"/mail/" ++ id) do {:ok, _} -> true ; {:error, _} -> false end end

end
