-module(iplookup_mnesia).
-behaviour(gen_server).

% API
-export([start_link/0]).

% Gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("iplookup.hrl").

-record(state, {ready=false,
	       ipv4worker,
	       ipv6worker}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    mnesia:create_table(ip_address, [{attributes, record_info(fields, ip_address)}, {ram_copies, [node()]}]),
    mnesia:add_table_index(ip_address, block_to),
    gen_server:cast(self(), {get_ips, 4}),
    gen_server:cast(self(), {get_ips, 6}),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({get_ips, 4}, #state{ipv4worker=Pid}=State) when is_pid(Pid) ->
    {noreply, State};
handle_cast({get_ips, 6}, #state{ipv6worker=Pid}=State) when is_pid(Pid) ->
    {noreply, State};
handle_cast({get_ips, 4}, State) ->
    {ok, Pid} = iplookup_fetcher:start_link("1"),
    monitor(process, Pid),
    {noreply, State#state{ipv4worker=Pid}};
handle_cast({get_ips, 6}, State) ->
    {ok, Pid} = iplookup_fetcher:start_link("7"),
    monitor(process,Pid),
    {noreply, State#state{ipv6worker=Pid}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, _Type, Pid, normal}, #state{ipv4worker=Pid}=State) ->
    {noreply, maybe_update_state(State#state{ipv4worker=undefined})};
handle_info({'DOWN', _Ref, _Type, Pid, normal}, #state{ipv6worker=Pid}=State) ->
    {noreply, maybe_update_state(State#state{ipv6worker=undefined})};
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

maybe_update_state(#state{ipv4worker=undefined,ipv6worker=undefined}=State) ->
    ets:insert(status, {status, true}),
    State;
maybe_update_state(State) ->
    State.

