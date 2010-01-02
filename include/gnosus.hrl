%% macro definitions
%%================================================================================
-define(DUMP(Args),
    io:format("~p~n", [Args])).

%%================================================================================
%% routes
-define(LOGIN, "/").
-define(REGISTER, "/register").
-define(ADMIN, "/admin").
-define(CLIENT, "/client").
-define(CLIENT_PROFILE, "/client/profile").
-define(HOST(H), "/host/"++H).
-define(HOST_ADD, "/host/add").
-define(HOST_USER_ADD(H), "/host/user/add/"++H).
-define(HOST_USER_REGISTER(H), "/host/user/register/"++H).
-define(HOSTS,"/hosts").
-define(PROFILE, "/profile").
-define(USER(E), "/user/"++E).
-define(USER_ADD,"/user/add").
