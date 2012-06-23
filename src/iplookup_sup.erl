-module(iplookup_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Port = list_to_integer(os:getenv("PORT")),
    Dispatch = [ {'_', [{[<<"country">>, ipaddress], iplookup_handler, [country]},
			{'_', iplookup_404_handler, []}]}
	       ],
    
    cowboy:start_listener(my_http_listener, 100,
			  cowboy_tcp_transport, [{port, Port}],
			  cowboy_http_protocol, [{dispatch, Dispatch}]),
    ets:new(status, [{read_concurrency,true},named_table,set,public]),
    {ok, {{one_for_one, 5, 10}, [
				 ?CHILD(iplookup_mnesia, worker)
				]}}.
