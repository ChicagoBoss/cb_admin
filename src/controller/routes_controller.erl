-module(routes_controller, [Req, SessionID]).
-compile(export_all).
-default_action(index).

before_(_) ->
    admin_lib:require_ip_address(Req).

index('GET', [], Authorization) ->
    {ok, [ {routes_section, true}, {all_routes, boss_web:get_all_routes()} ]}.

reload('GET', [], Authorization) ->
    boss_web:reload_routes(),
    lists:map(fun(Node) ->
                rpc:call(Node, boss_web, reload_routes, [])
        end, erlang:nodes()),
    boss_flash:add(SessionID, notice, "Routes reloaded"),
    {redirect, [{action, "index"}]}.
