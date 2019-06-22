defmodule CHAT do
  @moduledoc """
  `CHAT` is the front API module for application.
  It provides records defititions, KVX mentainformation and useful functions.
  """
  require Record
  require KVX

  Enum.each(
    Record.extract_all(from_lib: "chat/include/roster.hrl"),
    fn {name, definition} ->
      Record.defrecord(name, definition)
    end
  )

  def metainfo() do
    KVX.schema(
      name: :roster,
      tables: [
        KVX.table(
          name: CHAT.Pub,
          fields: Record.extract(:Pub, from_lib: "chat/include/roster.hrl")
        )
      ]
    )
  end

  def bin(key), do: :erlang.list_to_binary(:io_lib.format("~p", [key]))

  def user(id) do
    case :kvx.get(:writer, id) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end
end
