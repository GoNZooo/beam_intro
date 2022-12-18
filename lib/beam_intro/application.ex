defmodule BeamIntro.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # This starts a worker outside of the "convenient" Elixir style
      %{
        id: :beam_intro_lister_supervisor,
        start: {:beam_intro_lister_supervisor, :start_link, []}
      },
      {BeamIntro.Lister.Supervisor, []}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: BeamIntro.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
