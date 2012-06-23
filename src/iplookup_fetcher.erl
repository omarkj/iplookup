-module(iplookup_fetcher).
-behaviour(gen_server).

-record(state, {url}).

-include("iplookup.hrl").

% API
-export([start_link/1]).

% Gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
    gen_server:cast(self(), start),
    Url = "http://software77.net/geo-ip/?DL=" ++ Id,
    io:format("URL ~p~n", [Url]),
    {ok, #state{url="http://software77.net/geo-ip/?DL=" ++ Id}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(start, #state{url=Url}=State) ->
    {ok, {_, _, Body}} = httpc:request(Url),
    ok = parse(binary:split(zlib:gunzip(Body), <<"\n">>, [global])),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

parse([]) ->
    ok;
parse([<<"#", _/binary>>|Rest]) ->
    parse(Rest);
parse([<<>>]) ->
    ok;
parse([Msg|Rest]) ->
    Line = binary:split(Msg, <<",">>, [global]),
    Record = create_record(Line),
    save_record(Record),
    parse(Rest).

save_record(#ip_address{}=Record) ->
    mnesia:dirty_write(Record).

% IPv6
create_record([IpRange, CountryCode, Nic, _Timestamp]) ->
    IpRange0 = IpRange,
    [IpFrom, IpTo] = binary:split(IpRange0, <<"-">>),
    {ok, From} = inet_parse:address(binary_to_list(IpFrom)),
    {ok, To} = inet_parse:address(binary_to_list(IpTo)),
    #ip_address{block_from=From,
		block_to=To,
		nic=Nic,
		country_code=CountryCode};
% IPv4
create_record([IpFrom, IpTo, Nic, _Timestamp, CountryCode, _CCLong, _CName]) ->
    {ok, From} = inet_parse:address(binary_to_list(unqoute(IpFrom))),
    {ok, To} = inet_parse:address(binary_to_list(unqoute(IpTo))),
    #ip_address{block_from=From,
		block_to=To,
		nic=unqoute(Nic),
		country_code=unqoute(CountryCode)}.

unqoute(Binary) ->
    [_, Res, _] = binary:split(Binary, <<"\"">>, [global]),
    Res.
