%% macro definitions
%%================================================================================
-define(DUMP(Args),
    io:format("~p~n", [Args])).

%%================================================================================
%% paths
-define(LOGIN, "/").
-define(REGISTER, "/register").
-define(PROFILE, "/profile").
-define(ADMIN, "/admin").
-define(USER(E), "/user/"++E).
-define(USER_ADD,"/user/add").
-define(HOSTS,"/hosts").
-define(HOST(H), "/host/"++H).
-define(HOST_ADD, "/host/add").
-define(HOST_USER_ADD(H), "/host/user/add/"++H).
-define(HOST_USER_REGISTER(H), "/host/user/register/"++H).
-define(CLIENT, "/client").
-define(CLIENT_PROFILE, "/client/profile").
