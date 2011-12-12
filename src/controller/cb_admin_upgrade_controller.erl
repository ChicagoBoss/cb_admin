-module(cb_admin_upgrade_controller, [Req, SessionID]).
-compile(export_all).
-default_action(upgrade).

upgrade('GET', [], Auth) ->
    {ok, [ {upgrade_section, true} ]};
upgrade('POST', [], Auth) ->
    error_logger:info_msg("Reloading modules...~n"),
    boss_load:reload_all(),
    error_logger:info_msg("Reloading routes...~n"),
    boss_web:reload_routes(),
    error_logger:info_msg("Reloading translation...~n"),
    boss_web:reload_all_translations(),
    lists:map(fun(Node) ->
                error_logger:info_msg("Reloading on ~p~n", [Node]),
                rpc:call(Node, boss_load, reload_all, []),
                rpc:call(Node, boss_web, reload_routes, []),
                rpc:call(Node, boss_web, reload_all_translations, [])
        end, erlang:nodes()),
    {redirect, [{action, "upgrade"}]}.

reread_init_scripts('POST', [], Auth) ->
    boss_web:reload_init_scripts(),
    {redirect, [{action, "upgrade"}]}.

