defmodule CHAT.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :chat,
      version: "3.7.5",
      description: "CHAT Instant Messaging wss://n2o.im",
      package: package(),
      elixir: "~> 1.7",
      compilers: [:asn1] ++ Mix.compilers(),
      asn1_paths: ["src"],
      deps: deps()
    ]
  end

  def package() do
    [
      files: ~w(doc priv lib src mix.exs LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/o7/chat"}
    ]
  end

  def application(),
    do: [mod: {CHAT.Application, []}, applications: [:ranch, :cowboy, :kvs, :syn, :n2o]]

  def deps() do
    [
      {:ex_doc, "~> 0.20.2", only: :dev},
      {:asn1ex, github: "vicentfg/asn1ex", only: :dev},
      {:cowboy, "~> 2.5"},
      {:rocksdb, "~> 1.2.0"},
      {:syn, "~> 1.6.3"},
      {:n2o, "~> 6.8.1"},
      {:kvs, "~> 6.7.4"}
    ]
  end
end
