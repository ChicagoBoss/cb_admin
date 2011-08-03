-module(cb_admin_admin_controller, [Req, SessionID]).
-compile(export_all).
-default_action(index).

before_("splash") ->
    ok;
before_("access_denied") ->
    ok;
before_(_) ->
    cb_admin_lib:require_ip_address(Req).

index('GET', [], Authorization) ->
    [{loaded, ModulesLoaded}, _, _, _, _, _] = application:info(),
    ConfigValues = [ [{Key, Value}] || {Key, Value} <- application:get_all_env()],
    SystemValues = [ {otp_release, erlang:system_info(system_version)},
        {processors, erlang:system_info(logical_processors_online)},
        {uptime, cb_admin_lib:uptime()},
        {node, erlang:node()}
    ],
    {ok, [ {index_section, true}, {modules_loaded, ModulesLoaded}, 
            {config_env, ConfigValues}, {system_env, SystemValues},
            {nodes, erlang:nodes()}] }.

news_api('POST', ["created", Id], Auth) ->
    ok = boss_news:created(Id, Req:post_params("new")),
    {output, "ok"};
news_api('POST', ["updated", Id], Auth) ->
    ok = boss_news:updated(Id, Req:post_params("old"), Req:post_params("new")),
    {output, "ok"};
news_api('POST', ["deleted", Id], Auth) ->
    ok = boss_news:deleted(Id, Req:post_params("old")),
    {output, "ok"}.
