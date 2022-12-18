-module(beam_intro_lister).

-export([start_link/0, init/1, add/1, get/0, handle_call/3,
         handle_cast/2, handle_info/2]).

-behaviour(gen_server).

start_link() ->
  % We specify how to start the process. In this case we don't use the
  % arguments coming in at all, and pass down only the symbol `ok` to
  % our `init` function, which is used for determining the initial
  % state of the process.
  gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).

add(Thing) ->
  gen_server:cast(?MODULE, {add, Thing}).

get() ->
  gen_server:call(?MODULE, get).

init(_Args) ->
  % The initial state of the process is a map where the key `contents`
  % is associated with an empty list.
  {ok, #{contents => []}}.

% We use pattern matching here to pull out the `contents` value so we
% can use it in our logic. When a thing is added, we prepend it to our
% internal list of things.
handle_cast({add, Thing}, #{contents := OldContents}) ->
  {noreply, #{contents => [Thing | OldContents]}}.

% When someone requests the contents of our process, we reply to their
% call with the `contents` value in our state.
handle_call(get, _From, #{contents := Contents} = State) ->
  {reply, Contents, State}.

handle_info(_Info, State) ->
  {noreply, State}.
