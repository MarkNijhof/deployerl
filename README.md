Deployerl
=========

Temp write-up, will cristalize

Goals
-----

Manage deployments of erlang applications on servers. Each server starts deployerl automatically in "client" mode with a spesific "role". This client will then call home (udp) to a deloyerl instance running in "server" mode. The "server" instance will then provide the "client" with a manifest containing the apps the client should manage, where to get the release package and how to deal with failures.

The server should keep track on what versions are currently running in production and adjust the balance between versions of the same app (partial blue green deployment 90% blue / 10% green ->)

Clients should be able to kill themselves when it detects an unhealthy state, servers should be able to kill clients

Server will periodically check a location for updated configuration, or new configuration can be pushed to it.
