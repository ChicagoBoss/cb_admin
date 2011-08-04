chicagoboss_admin
=================

This is the admin interface for Chicago Boss, which can be configured as a standalone application with CB 0.6 or later.

Installation as a standalone server
-----------------------------------

This isn't very useful, but it's easy. First make sure ChicagoBoss is built in the parent directory; otherwise modify start.sh and start-dev.sh to point to the ChicagoBoss ebin/ directory on your machine.

Then:

    mkdir log
    make
    ./start.sh

Then visit http://localhost:8001/

Installation with an existing CB server
---------------------------------------

First "make" and add the ebin/ directory to the path of your CB startup scripts (start.sh and start-dev.sh).

Add something like this to your boss.config:

    [{boss, [
        {applications, [cb_admin, ...]},
        ...
    ]},
    {cb_admin, [
        {allow_ip_blocks, ["127.0.0.1"]},
        {base_url, "/admin"}
    ]}].

