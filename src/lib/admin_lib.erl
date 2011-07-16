-module(admin_lib).

-export([push_update/3, uptime/0, mask_ipv4_address/2, encode_csv_value/1, parse_csv/1]).
-export([require_ip_address/1, lang_write_to_file/4]).

push_update(updated, {Record, Attr, OldVal, NewVal}, Channel) ->
    boss_mq:push(Channel, [{ev, updated}, {data, [{id, Record:id()}, {attr, Attr}, {val, NewVal}]}]).

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [D,H,M,S])).

mask_ipv4_address({I1, I2, I3, I4}, MaskInt) ->
    ((I1 bsl 24 + I2 bsl 16 + I3 bsl 8 + I4) band ((1 bsl 32) - (1 bsl (32 - MaskInt)))).

encode_csv_value(Val) when is_binary(Val) ->
    encode_csv_value(binary_to_list(Val));
encode_csv_value(Val) when is_atom(Val) ->
    encode_csv_value(atom_to_list(Val));
encode_csv_value({_, _, _} = Val) ->
    encode_csv_value(erlydtl_filters:date(calendar:now_to_datetime(Val), "F d, Y H:i:s"));
encode_csv_value({{_, _, _}, {_, _, _}} = Val) ->
    encode_csv_value(erlydtl_filters:date(Val, "F d, Y H:i:s"));
encode_csv_value(Val) ->
    encode_csv_value(Val, []).

encode_csv_value([], Acc) ->
    [$"|lists:reverse([$"|Acc])];
encode_csv_value([$"|T], Acc) ->
    encode_csv_value(T, [$", $" | Acc]);
encode_csv_value([H|T], Acc) ->
    encode_csv_value(T, [H|Acc]).


% Taken from http://blog.vmoroz.com/2011/01/csv-in-erlang.html
parse_csv(Data) when is_binary(Data) -> parse_csv(binary_to_list(Data));
parse_csv(Data) -> parse(Data, [], [], []).

parse([$\r|Data], Field, Fields, Lines) -> parse_r(Data, Field, Fields, Lines);
parse([$\n|Data], Field, Fields, Lines) -> parse(Data, [], [], [[Field|Fields]|Lines]);
parse([$,|Data], Field, Fields, Lines)  -> parse(Data, [], [Field|Fields], Lines);
parse([$"|Data], [], Fields, Lines)     -> parse_q(Data, [], Fields, Lines);
parse([C|Data], Field, Fields, Lines)   -> parse(Data, [C|Field], Fields, Lines);
parse([], [], [], Lines)                -> lists:reverse(
                                               [lists:reverse(
                                                 [lists:reverse(F) || F <- L]
                                               ) || L <- Lines]
                                             );
parse([], Field, Fields, Lines)         -> parse([], [], [], [[Field|Fields]|Lines]).

parse_r([$\n|_] = Data, Field, Fields, Lines) -> parse(Data, Field, Fields, Lines).

parse_q([$"|Data], Field, Fields, Lines) -> parse_qq(Data, Field, Fields, Lines);
parse_q([C|Data], Field, Fields, Lines)  -> parse_q(Data, [C|Field], Fields, Lines).

parse_qq([$"|Data], Field, Fields, Lines) -> parse_q(Data, [$"|Field], Fields, Lines);
parse_qq([C|_] = Data, Field, Fields, Lines)  
  when C == $,; C == $\r; C == $\n        -> parse(Data, Field, Fields, Lines);
parse_qq([], Field, Fields, Lines)        -> parse([], Field, Fields, Lines).

require_ip_address(Req) ->
    ClientIp = case Req:header(x_forwarded_for) of
        undefined -> Req:peer_ip();
        IP -> IP
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
                        admin_lib:mask_ipv4_address(ClientIp, MaskInt) =:= admin_lib:mask_ipv4_address(IPAddressTuple, MaskInt)
                end;
            (_, true) ->
                true
        end, false, boss_env:get_env(admin_ip_blocks, ["192.168.0.0/16", "127.0.0.1", "10.0.0.0/16"])),
    case Authorized of
        true ->
            {ok, local};
        _ ->
            {redirect, [{controller, "admin"}, {action, "access_denied"}]}
    end.

lang_write_to_file(IODevice, Original, Translation, BlockIdentifier) ->
	OriginalEncoded = unicode:characters_to_list(boss_lang:escape_quotes(Original)),
	TranslationEncoded = unicode:characters_to_list(boss_lang:escape_quotes(Translation)),
	case BlockIdentifier of
		undefined -> 
			file:write(IODevice, io_lib:format("\nmsgid \"~ts\"\n",[list_to_binary(OriginalEncoded)])),	   
			file:write(IODevice, io_lib:format("\msgstr \"~ts\"\n",[list_to_binary(TranslationEncoded)]));
		Identifier -> 
			file:write(IODevice, io_lib:format("\n#. ~ts\n",[list_to_binary(Identifier)])),
			file:write(IODevice, io_lib:format("msgid \"~s\"\n", [""])),
			{ok, OriginalTokens} = regexp:split(OriginalEncoded,"\r\n"),
			lang_write_multiline_to_file(IODevice, OriginalTokens),
			file:write(IODevice, io_lib:format("\msgstr \"~s\"\n", [""])),
			{ok, TranslationTokens} = regexp:split(TranslationEncoded,"\r\n"),
			lang_write_multiline_to_file(IODevice, TranslationTokens)
	end.

lang_write_multiline_to_file(IODevice, []) -> ok;
lang_write_multiline_to_file(IODevice, [Token|Rest]) ->
	ParsedToken = case Token of
		[] -> "";
		_ -> Token
	end,
	case Rest of
		[] -> file:write(IODevice, io_lib:format("\"~ts\"\n", [list_to_binary(ParsedToken)]));
		_ -> file:write(IODevice, io_lib:format("\"~ts~c~c\"\n", [list_to_binary(ParsedToken), 92, 110]))
	end,
	lang_write_multiline_to_file(IODevice, Rest).
