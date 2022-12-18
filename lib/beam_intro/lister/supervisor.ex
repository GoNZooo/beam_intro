defmodule BeamIntro.Lister.Supervisor do
  use Supervisor

  def start_link(_args) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    children = [
      {BeamIntro.Lister, []}
    ]

    Supervisor.init(
      children,
      # We specify here that the supervisor restarts a single process
      # when it dies, not all the processes attached to the supervisor.
      strategy: :one_for_one,
      # If 3 children die in 5 seconds, the supervisor will terminate.
      # If this supervisor is a child of another supervisor, the parent
      # supervisor will react to that termination as specified.
      max_restarts: 3,
      max_seconds: 5
    )
  end
end
