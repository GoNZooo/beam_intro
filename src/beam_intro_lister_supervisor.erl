-module(beam_intro_lister_supervisor).

-export([start_link/0, init/1]).

-behavior(supervisor).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Flags =
    #{% We specify here that the supervisor restarts a single process when it
      % dies, not all the processes attached to the supervisor.
      strategy => one_for_one,
      % If 3 children die in 5 seconds, the supervisor will terminate. If this
      % supervisor is a child of another supervisor, the parent supervisor will
      % react to that termination as specified.
      intensity => 3,
      period => 5},
  % This describes how our child process is started.
  ListerSpec =
    #{id => beam_intro_lister,
      start => {beam_intro_lister, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker},
  {ok, {Flags, [ListerSpec]}}.
