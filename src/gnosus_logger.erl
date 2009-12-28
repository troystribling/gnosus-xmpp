%% logging
%%--------------------------------------------------------------------------------
-module(gnosus_logger).
 
%% API
-export([
	message/1,
    warning/1,
    alarm/1
	]).
 
%%================================================================================
message({starting, M}) ->
    error_logger:info_msg("starting: ~p~n", [M]);
 
message({started, M}) ->
    error_logger:info_msg("started: ~p~n", [M]);

message({stoping, M}) ->
    error_logger:info_msg("stopping: ~p~n", [M]);
 
message({authenticated, M}) ->
    error_logger:info_msg("starting new authenticated session: ~p~n", [M]);

message({terminate_session, M}) ->
    error_logger:info_msg("terminated session: ~p~n", [M]);

message({admin_authorized, M}) ->
    error_logger:info_msg("admin authorized for request: ~p, from: ~p~n", M);

message({apply_method_succeeded, M}) ->
    error_logger:info_msg("apply method succeeded for method: ~p, with arguments: ~p~n", M);

message({add_host_succeeded, M}) ->
    error_logger:info_msg("add host: ~p, transaction succeeded~n", [M]);

message({add_host_ui_succeeded, M}) ->
    error_logger:info_msg("add host succeeded for host: ~p and user:~p~n", M);

message({add_host_and_user_succeeded, M}) ->
    error_logger:info_msg("add host and users transaction succeeded for host: ~p and user: ~p~n", M);

message({remove_host_succeeded, M}) ->
    error_logger:info_msg("remove host transaction succeededn for host: ~p~n", [M]);

message({remove_host_ui_succeeded, M}) ->
    error_logger:info_msg("remove host succeeded for host: ~p and user:~p~n", M);

message({remove_host_and_users_succeeded, M}) ->
    error_logger:info_msg("remove host and users transaction succeeded for host: ~p and user: ~p~n", M);

message({add_host_user_succeeded, M}) ->
    error_logger:info_msg("add user: ~p, succeeded for host: ~p~n", M);

message({remove_host_user_succeeded, M}) ->
    error_logger:info_msg("remove user: ~p, succeeded for host: ~p~n", M);

message({add_user_succeeded, M}) ->
    error_logger:info_msg("add user: ~p, succeeded", [M]);

message({remove_user_succeeded, M}) ->
    error_logger:info_msg("remove user: ~p, succeeded", [M]);

message({user_registered, M}) ->
    error_logger:info_msg("user registered: ~p~n", [M]);

message({user_registeration_succeeded, M}) ->
    error_logger:info_msg("user registeration succeeded: ~p~n", [M]);

message({host_user_registration_succeeded, M}) ->
    error_logger:info_msg("user registeration succeeded for user: ~p host: ~p and admin~p~n", [M]);

message({config_loaded, M}) ->
    error_logger:info_msg("configuration loaded: ~p~n", [M]);

message({rollback_succeeded, M}) ->
    error_logger:info_msg("rollback succeeded: ~p~n", [M]);

message({rollback_starting, M}) ->
    error_logger:info_msg("rollback starting: ~p~n", [M]);

message(X) ->
    error_logger:info_msg("~p~n", [X]).
 
%%--------------------------------------------------------------------------------
warning({mnesia_start_timeout, T}) ->
    error_logger:warning_msg("mnesia timeout waiting for tables: ~p~n", T);
 
warning({mnesia_start_error, M}) ->
    error_logger:warning_msg("mnesia start error: ~p~n", [M]);
 
warning({authentication_failed, M}) ->
    error_logger:warning_msg("authentication failed for: ~p~n", [M]);

warning({authentication_authorization_failed, M}) ->
    error_logger:warning_msg("authentication authorization failed for request: ~p~n", [M]);

warning(X) ->
    error_logger:warning_msg("~p~n", [X]).
 
%%--------------------------------------------------------------------------------
alarm({apply_method_failed, M}) ->
    error_logger:error_msg("apply method failed for method: ~p, with arguments: ~p~n", M);

alarm({add_host_failed, M}) ->
    error_logger:error_msg("add host: ~p, transaction failed, starting rollback~n", [M]);

alarm({remove_host_failed, M}) ->
    error_logger:error_msg("remove host: ~p, transaction failed, starting rollback~n", [M]);

alarm({add_host_and_user_failed, M}) ->
    error_logger:error_msg("add host and user transaction failed for host: ~p and user: ~p, starting rollback~n", M);

alarm({remove_host_and_users_failed, M}) ->
    error_logger:error_msg("remove host and user transaction failed for host: ~p and user: ~p~n", M);
        
alarm({add_host_user_failed, M}) ->
    error_logger:error_msg("add user: ~p, failed for host: ~p~n", M);

alarm({remove_host_user_failed, M}) ->
    error_logger:error_msg("remove user: ~p, failed for host: ~p~n", M);

alarm({add_user_failed, M}) ->
    error_logger:error_msg("user: ~p, creation failed", [M]);

alarm({remove_user_failed, M}) ->
    error_logger:error_msg("user: ~p, deletion failed", [M]);

alarm({host_user_deletion_invalid, M}) ->
    error_logger:error_msg("user: ~p, does not own host: ~p and tried to delete user~n", M);

alarm({host_and_client_user_database_update_failed, M}) ->
    error_logger:error_msg("host and cleint user database update failed for host: ~p, and user~p~n", M);

alarm({client_user_database_update_failed, M}) ->
    error_logger:error_msg("client user database update failed for host: ~p, and user~p~n", M);

alarm({host_access_invalid, M}) ->
    error_logger:error_msg("user: ~p, does not own host: ~p and tried to modify it: ~p~n", M);

alarm({admin_authorization_failed, M}) ->
    error_logger:error_msg("admin authorization failed for request: ~p from: ~p~n", M);

alarm({config_load_failed, M}) ->
    error_logger:error_msg("config load failed: ~p~n", [M]);

alarm({rollback_failed, M}) ->
    error_logger:error_msg("rollback failed for methods:~p with aruments: ~p~n", [M]);

alarm(X) ->
    error_logger:error_msg("~p~n", [X]).
 
 
