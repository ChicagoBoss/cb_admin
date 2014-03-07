cb_admin
=================

This is the admin interface for Chicago Boss, which can be configured as a standalone application with CB 0.6 or later.

Installation as a standalone server
-----------------------------------

Check your boss.config and make sure that the path's are correct, defaults to ./deps/boss (framework) and ../cb_admin (the checkout dir).

Then in *nix:

    ./rebar get-deps compile
    ./init.sh start

In Windows:
	
	rebar get-deps compile
	start-server.bat

Then visit http://localhost:8001/admin

Installation with an existing CB server
---------------------------------------

Add to rebar.config 
```erlang
{deps, [
  % ...
  {cb_admin, ".*", {git, "git://github.com/ChicagoBoss/cb_admin.git", "HEAD"}}
]}.
```

And add something like this to your boss.config:

    [{boss, [
        {applications, [cb_admin, ...]},
        ...
    ]},
    {cb_admin, [
        {path, "./deps/cb_admin"},
        {allow_ip_blocks, ["127.0.0.1"]},
        {base_url, "/admin"}
    ]}].

Run to fetch and compile cb_admin

```console
./rebar get-deps compile
./init-dev.sh
```

