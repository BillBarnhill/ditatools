
-module(mapgen_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

worker(Mod) ->
	{Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}.

init([]) ->
    {ok, { {one_for_one, 5, 10}, [worker(mapgen_server)]} }.

