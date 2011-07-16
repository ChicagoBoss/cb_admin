-module(cb_admin_lang_lib).
-export([lang_write_to_file/4]).

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
