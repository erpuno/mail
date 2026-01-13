defmodule MAIL.Mixfile do
  use Mix.Project

  def project do
      [
        app: :mail,
        version: "9.1.2",
        description: "MAIL Simple Message Delivery Protocol",
        package: package(),
        deps: deps(),
        releases: [mail: [include_executables_for: [:unix], cookie: "SYNRC:MAIL"]]
      ]
  end

  def application do
      [ mod: { MAIL.Application, [] },
        extra_applications: [ :syn, :n2o, :mnesia, :kvs ]
      ]
  end

  def package do
      [
        files: ~w(include lib priv src LICENSE mix.exs README.md),
        licenses: ["ISC"],
        maintainers: ["Namdak Tonpa"],
        name: :mail420,
        links: %{"GitHub" => "https://github.com/erpuno/mail"}
      ]
  end

  def deps do
      [
        {:ex_doc, ">= 0.0.0", only: :dev},
        {:telemetry, "~> 1.3.0"},
        {:bandit, "~> 1.0"},
        {:websock_adapter, "~> 0.5"},
        {:syn, "~> 2.1.1"},
        {:n2o, "~> 10.8.2"},
        {:kvs, "~> 10.8.2"}
      ]
  end

end
