defmodule BeamIntro.Lister do
  use GenServer

  def start_link(_args) do
    # We specify how to start the process. In this case we don't use
    # the arguments coming in at all, and pass down only the symbol
    # `ok` to our `init` function, which is used for determining the
    # initial state of the process.
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add(thing) do
    GenServer.cast(__MODULE__, {:add, thing})
  end

  def get() do
    GenServer.call(__MODULE__, :get)
  end

  def init(:ok) do
    # The initial state of the process is a map where the key
    # `contents` is associated with an empty list.
    {:ok, %{contents: []}}
  end

  # We use pattern matching here to pull out the `contents` value
  # so we can use it in our logic. When a thing is added, we prepend
  # it to our internal list of things.
  def handle_cast({:add, thing}, %{contents: contents} = state) do
    {:noreply, %{state | contents: [thing | contents]}}
  end

  # When someone requests the contents of our process, we reply to
  # their call with the `contents` value in our state.
  def handle_call(:get, _from, %{contents: contents} = state) do
    {:reply, contents, state}
  end
end
