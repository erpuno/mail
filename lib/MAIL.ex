defmodule MAIL do
  @moduledoc """
  `MAIL` is the front API module for application.
  It provides records defititions, KVS mentainformation and useful functions.
  """
  require Record
  require KVS

  Enum.each(
    Record.extract_all(from_lib: "mail/priv/ROSTER.hrl"),
    fn {name, definition} ->
      Record.defrecord(name, definition)
    end
  )

  def metainfo() do
    KVS.schema(
      name: :roster,
      tables: [
        KVS.table(
          name: MAIL.Pub,
          fields: Record.extract(:Pub, from_lib: "mail/priv/ROSTER.hrl")
        )
      ]
    )
  end

  def bin(key), do: :erlang.list_to_binary(:io_lib.format("~p", [key]))

  def user(id) do
    case :kvs.get(:writer, '/mail/' ++ id) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end
end
