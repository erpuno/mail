defmodule MAIL.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :chat,
      version: "5.11.1",
      description: "MAIL Message Delivery System wss://mail-1.erp.uno:8042/",
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
      links: %{"GitHub" => "https://github.com/erpuno/mail"}
    ]
  end

  def application(),
    do: [mod: {MAIL.Application, []}, applications: [:ranch, :cowboy, :kvs, :syn, :n2o]]

  def deps() do
    [
      {:ex_doc, "~> 0.20.2", only: :dev},
      {:asn1ex, github: "vicentfg/asn1ex", only: :dev},
      {:cowboy, "~> 2.8"},
      {:rocksdb, "~> 1.6.0"},
      {:syn, "~> 2.1.1"},
      {:n2o, "~> 8.8.1"},
      {:kvs, "~> 8.10.4"}
    ]
  end
end
