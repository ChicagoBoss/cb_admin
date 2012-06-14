-module(cb_admin_lib).

-export([push_update/3, uptime/0, mask_ipv4_address/2, require_ip_address/1]).

push_update(updated, {Record, Attr, OldVal, NewVal}, Channel) ->
    boss_mq:push(Channel, [{ev, updated}, {data, [{id, Record:id()}, {attr, Attr}, {val, NewVal}]}]).

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [D,H,M,S])).

mask_ipv4_address({I1, I2, I3, I4}, MaskInt) ->
    ((I1 bsl 24 + I2 bsl 16 + I3 bsl 8 + I4) band ((1 bsl 32) - (1 bsl (32 - MaskInt)))).

require_ip_address(Req) ->
    ClientIp = case Req:header(x_forwarded_for) of
        undefined -> Req:peer_ip();
        IP when is_tuple(IP)-> IP;
        IP when is_list(IP) -> list_to_tuple(lists:map(fun erlang:list_to_integer/1, string:tokens(IP, ".")))
    end,
    Authorized = lists:foldr(fun
            (IPBlock, false) ->
                case string:tokens(IPBlock, "/") of
                    [IPAddress] ->
                        IPAddress =:= string:join(lists:map(fun erlang:integer_to_list/1, 
                                tuple_to_list(ClientIp)), ".");
                    [IPAddress, Mask] ->
                        MaskInt = list_to_integer(Mask),
                        IPAddressTuple = list_to_tuple(lists:map(fun erlang:list_to_integer/1, string:tokens(IPAddress, "."))),
                        mask_ipv4_address(ClientIp, MaskInt) =:= mask_ipv4_address(IPAddressTuple, MaskInt)
                end;
            (_, true) ->
                true
        end, false, boss_env:get_env(cb_admin, allow_ip_blocks, 
            ["192.168.0.0/16", "127.0.0.1", "10.0.0.0/16"])),
    case Authorized of
        true ->
            {ok, local};
        _ ->
            {redirect, [{controller, "admin"}, {action, "access_denied"}]}
    end.
