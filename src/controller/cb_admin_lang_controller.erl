-module(cb_admin_lang_controller, [Req, SessionID]).
-compile(export_all).
-default_action(show).

before_(_) ->
    cb_admin_lib:require_ip_address(Req).

show('GET', [], Auth) ->
    Applications = boss_web:get_all_applications(),
    {ok, [{lang_section, true}, {applications, Applications}]};
show('GET', [App], Auth) ->
    Applications = boss_web:get_all_applications(),
    Languages = boss_files:language_list(list_to_atom(App)),
    {ok, [{lang_section, true}, {this_application, App},
            {applications, Applications}, {languages, Languages}]};
show('GET', [App, Lang], Auth) ->
    OriginalLang = boss_env:get_env(assume_locale, "en"),
    AppAtom = list_to_atom(App),
    Applications = boss_web:get_all_applications(),
    Languages = boss_files:language_list(AppAtom),
    {Untranslated, Translated} = boss_lang:extract_strings(AppAtom, Lang),
    LastModified = filelib:last_modified(boss_files:lang_path(AppAtom, Lang)),
    {ok, [{this_lang, Lang}, {languages, Languages},
            {this_application, App}, {applications, Applications},
            {original_lang, OriginalLang},
            {untranslated_messages, Untranslated},
            {translated_messages, Translated},
            {last_modified, LastModified},
		  	{lang_section, true}],
        [{"Cache-Control", "no-cache"}]}.

edit('POST', [App, Lang|Fmt], Auth) ->
    WithBlanks = Req:post_param("trans_all_with_blanks"),
    AppAtom = list_to_atom(App),
    LangFile = boss_files:lang_path(AppAtom, Lang),
    {ok, IODevice} = file:open(LangFile, [write, append]),
    lists:map(fun(Message) ->
                Original = proplists:get_value("orig", Message),
                Translation = proplists:get_value("trans", Message),
                case Translation of
                    "" -> 
                        case WithBlanks of
                            undefined -> ok;
                            _ -> boss_lang:lang_write_to_file(IODevice, Original, Translation)
                        end;
                    _ -> boss_lang:lang_write_to_file(IODevice, Original, Translation)
                end
        end, Req:deep_post_param(["messages"])),
    file:close(IODevice),
    Trans_pid=boss_web:translator_pid(AppAtom),
    boss_translator:reload(Trans_pid,Lang),
    case Fmt of
        ["json"] -> {json, [{success, true}]};
        [] -> {redirect, [{action, "show"}, {app, App}, {lang, Lang}]}
    end.

create('GET', [], Auth) ->
    {ok, [{lang_section, true}, {applications, boss_web:get_all_applications()}]};
create('GET', [App], Auth) ->
    AppAtom = list_to_atom(App),
    {ok, [{lang_section, true}, {applications, boss_web:get_all_applications()},
            {this_application, App}, {languages, boss_files:language_list(AppAtom)}]};
create('POST', [App], Auth) ->
    % TODO sanitize
    AppAtom = list_to_atom(App),
    NewLang = Req:post_param("language"),
    boss_lang:create_lang(AppAtom, NewLang),
    {redirect, [{action, "show"}, {app, App}, {lang, NewLang}]}.

delete('GET', [App, Lang], Auth) ->
    {ok, [{lang_section, true}, {this_application, App}, {applications, boss_web:get_all_applications()},
            {this_lang, Lang}]};
delete('POST', [App, Lang], Auth) ->
    AppAtom = list_to_atom(App),
    boss_lang:delete_lang(AppAtom, Lang),
    boss_web:reload_translation(Lang),
    {redirect, [{action, "show"}]}.

big_red_button('GET', [App], Auth) ->
    AppAtom = list_to_atom(App),
    Languages = lists:map(fun(Lang) ->
                {Untranslated, Translated} = boss_lang:extract_strings(AppAtom, Lang),
                [{code, Lang}, {untranslated_strings, Untranslated}]
        end, boss_files:language_list(AppAtom)),
    {ok, [{lang_section, true}, {this_application, App}, {applications, boss_web:get_all_applications()},
            {languages, Languages}, {strings, boss_lang:extract_strings(AppAtom)}]}.

