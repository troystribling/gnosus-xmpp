%% macro definitions
%%================================================================================
-define(DUMP(Args),
    io:format("~p~n", [Args])).

%%================================================================================
%% constants
-define(EJABBERD, 'ejabberd@ubuntu').
-define(EJABBERD_ROOT_DOMAIN, "ubuntu").
-define(TLD, ".local").
