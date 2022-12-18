defmodule BeamIntroTest do
  use ExUnit.Case
  doctest BeamIntro

  test "greets the world" do
    assert BeamIntro.hello() == :world
  end
end
