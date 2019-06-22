defmodule CHAT do
  require Record

  Enum.each(Record.extract_all(from_lib: "chat/include/roster.hrl"), fn {name, definition} ->
    Record.defrecord(name, definition)
  end)

end
