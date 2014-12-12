-module(mod___collectd).

-export([add_collectd_server_ip_address/1]).
-export([remove_collectd_server_ip_address/1]).
-export([clear_any_collectd_server_ip_addresses/0]).

%% --------------------------------------------------%%
%% API
%% --------------------------------------------------%%

add_collectd_server_ip_address(IpAddress) ->
    case filelib:is_file("/etc/collectd/collectd.conf") of
        false ->
            ok;
        true ->
            Cmd = ""
                ++"sudo sed -i.bak "
                ++"'s/servers_do_not_remove"
                ++"/servers_do_not_remove\\n    Server \""
                ++ binary_to_list(IpAddress)
                ++"\" \"25826\"/g' "
                ++"/etc/collectd/collectd.conf"
                ++" && "
                ++"sudo /etc/init.d/collectd restart",

            os:cmd(Cmd),
            ok
    end.

remove_collectd_server_ip_address(IpAddress) ->
    case filelib:is_file("/etc/collectd/collectd.conf") of
        false ->
            ok;
        true ->
            [Ip1, Ip2, Ip3, Ip4] = binary:split(IpAddress, <<".">>, [global]),

            Cmd = ""
                ++"sudo sed -ni.bak "
                ++"'/    Server \""
                ++ binary_to_list(Ip1) ++"\."
                ++ binary_to_list(Ip2) ++"\."
                ++ binary_to_list(Ip3) ++"\."
                ++ binary_to_list(Ip4) ++"\" \"25826\"/!p' "
                ++"/etc/collectd/collectd.conf"
                ++" && "
                ++"sudo /etc/init.d/collectd restart",

            os:cmd(Cmd),
            ok
    end.

clear_any_collectd_server_ip_addresses() ->
    case filelib:is_file("/etc/collectd/collectd.conf") of
        false ->
            ok;
        true ->
            Cmd = ""
                ++"sudo sed -i.bak "
                ++"'/^    Server \"/d' "
                ++"/etc/collectd/collectd.conf"
                ++" && "
                ++"sudo /etc/init.d/collectd restart",

            os:cmd(Cmd),
            ok
    end.
