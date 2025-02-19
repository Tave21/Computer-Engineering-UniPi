%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% This is the fleetfra_chin top-level supervisor.
%% This module defines a supervisor for the fleetfra_chin system.
%% @end
%% Created : 29. Jan 2025 10:30
%%==============================================================================%%

-module(fleetfra_chin_sup).
-author("SaveMos").
-behaviour(supervisor).

-export([start_link/0]).  % Export the start_link function to initiate the supervisor
-export([init/1]).        % Export the init function to initialize the supervisor

-define(SERVER, ?MODULE).  % Define SERVER as the module name for easy reference

%%%===================================================================
%%% Supervisor API functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Starts the supervisor process.
%% @returns {ok, Pid} where Pid is the process identifier of the supervisor.
%% @end
%%-------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor Callback functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Initializes the supervisor by setting up flags and child specifications.
%% @param [] The list of arguments (empty in this case).
%% @returns {ok, {SupFlags, ChildSpecs}} where SupFlags are the supervisor flags
%%          and ChildSpecs are the child specifications (empty here).
%% @end
%%-------------------------------------------------------------------
init([]) ->
  % Define supervisor flags and child specifications
  SupFlags = #{strategy => one_for_one,   % Supervisor strategy: each child is managed independently
    intensity => 3,                       % Maximum 3 restarts within the defined period
    period => 5},                         % Period in seconds (not used in this case)

  % No child processes to supervise, so we define an empty list of child specifications
  ChildSpecs = [],

  {ok, {SupFlags, ChildSpecs}}.  % Return the supervisor configuration, indicating successful initialization

%%%===================================================================
%%% Internal Functions
%%%===================================================================
