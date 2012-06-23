-module(iplookup_handler).

% http handler
-export([init/3, terminate/2]).

% Rest
-export([rest_init/2, allowed_methods/2, malformed_request/2,
	 resource_exists/2, content_types_provided/2, service_available/2]).

% Returns
-export([to_html/2,
	 to_text/2,
	 to_json/2]).

-include("iplookup.hrl").

-record(state, {req_type :: country,
		ip :: tuple()|undefined,
		country :: binary()|undefined}).

init({_Any, http}, _Req, _State) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, [Type]) ->
    {ok, Req, #state{req_type=Type}}.

service_available(Req, State) ->
    case ets:lookup(status, status) of
	[{status,true}] ->
	    {true, Req, State};
	_ ->
	    {false, Req, State}
    end.

allowed_methods(Req, State) ->
    {['GET','HEAD'], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"html">>, []}, to_html},
      {{<<"text">>, <<"plain">>,[]}, to_text},
      {{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

malformed_request(Req, State) ->
    {Ipaddress, Req0} = cowboy_http_req:binding(ipaddress, Req),
    IpStr = binary_to_list(Ipaddress),
    case inet_parse:address(IpStr) of
	{ok, IpTuple} ->
	    {false, Req0, State#state{ip=IpTuple}};
	{error, _} ->
	    {true, Req0, State}
    end.

resource_exists(Req, #state{ip=Ip}=State) ->
    case mnesia:dirty_select(ip_address,
			     [{#ip_address{block_from = '$1',
					   block_to = '$2',nic = '_',
					   country_code = '$3'},
			       [{'=<','$1',{const,Ip}},
				{'>=','$2',{const,Ip}}],
			       ['$3']}]) of
	[] ->
	    {false, Req, State};
	[Res] ->
	    {true, Req, State#state{country=Res}}
    end.

to_html(Req, #state{country=Country}=State) ->
    {Country, Req, State}.

to_text(Req, #state{country=Country}=State) ->
    {Country, Req, State}.

to_json(Req, #state{country=Country}=State) ->
    {<<"{\"country\":\"", Country/binary, "\"}">>, Req, State}.

terminate(_Req, _State) ->
    ok.
