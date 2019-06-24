defmodule CHAT.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :chat,
      version: "3.6.0",
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
      files: ~w(doc include priv lib src mix.exs LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/o7/chat"}
    ]
  end

  def application(),
    do: [mod: {CHAT.Application, []}, applications: [:ranch, :cowboy, :n2o, :kvs, :syn]]

  def deps() do
    [
      {:ex_doc, "~> 0.11", only: :dev},
      {:asn1ex, github: "vicentfg/asn1ex", only: :dev},
      {:cowboy, "~> 2.5"},
      {:rocksdb, "~> 1.2.0"},
      {:syn, "~> 1.6.3"},
      {:kvs, "~> 6.6"},
      {:n2o, "~> 6.6"}
    ]
  end
end
