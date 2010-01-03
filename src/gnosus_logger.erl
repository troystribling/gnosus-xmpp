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
    log_message("starting: ~p~n", [M]);
 
message({started, M}) ->
    log_message("started: ~p~n", [M]);

message({stoping, M}) ->
    log_message("stopping: ~p~n", [M]);
 
message({authenticated, M}) ->
    log_message("starting new authenticated session for: ~p~n", [M]);

message({authentication_user_not_found, M}) ->
    log_message("user not found in user database during authentication: ~p~n", [M]);

message({authentication_client_user_not_found, M}) ->
    log_message("user not found in client user database during authentication: ~p~n", [M]);

message({terminate_session, M}) ->
    log_message("terminated session: ~p~n", [M]);

message({admin_authorized, M}) ->
    log_message("admin authorized for request: ~p, from: ~p~n", M);

message({apply_method_succeeded, M}) ->
    log_message("apply method succeeded for method: ~p, with arguments: ~p~n", M);

message({add_host_succeeded, M}) ->
    log_message("add host: ~p, transaction succeeded~n", [M]);

message({add_host_ui_succeeded, M}) ->
    log_message("add host succeeded for host: ~p and user:~p~n", M);

message({add_host_and_user_succeeded, M}) ->
    log_message("add host and users transaction succeeded for host: ~p and user: ~p~n", M);

message({remove_host_succeeded, M}) ->
    log_message("remove host transaction succeededn for host: ~p~n", [M]);

message({remove_host_ui_succeeded, M}) ->
    log_message("remove host succeeded for host: ~p and user:~p~n", M);

message({remove_host_and_users_succeeded, M}) ->
    log_message("remove host and users transaction succeeded for host: ~p and user: ~p~n", M);

message({add_host_user_succeeded, M}) ->
    log_message("add user: ~p, succeeded for host: ~p~n", M);

message({remove_host_user_succeeded, M}) ->
    log_message("remove user: ~p, succeeded for host: ~p~n", M);

message({add_user_succeeded, M}) ->
    log_message("add user: ~p, succeeded~n", [M]);

message({update_user_succeeded, M}) ->
    log_message("update user: ~p, succeeded~n", [M]);

message({update_client_user_succeeded, M}) ->
    log_message("update client user: ~p, succeeded~n", [M]);

message({remove_user_succeeded, M}) ->
    log_message("remove user: ~p, succeeded~n", [M]);

message({user_registered, M}) ->
    log_message("user registered: ~p~n", [M]);

message({user_registeration_succeeded, M}) ->
    log_message("user registeration succeeded: ~p~n", [M]);

message({host_user_registration_succeeded, M}) ->
    log_message("host user registration succeeded for user: ~p and host: ~p~n", M);

message({host_user_add_succeeded, M}) ->
    log_message("host user add succeeded for host: ~p and user: ~p~n", M);

message({host_user_remove_succeeded, M}) ->
    log_message("host user remove succeeded for host: ~p and user: ~p~n", M);

message({config_loaded, M}) ->
    log_message("configuration loaded: ~p~n", [M]);

message({rollback_succeeded, M}) ->
    log_message("rollback succeeded: ~p~n", [M]);

message({rollback_starting, M}) ->
    log_message("rollback starting: ~p~n", [M]);

message(X) ->
    log_message("~p~n", [X]).
 
%%================================================================================
warning({mnesia_start_timeout, T}) ->
    log_warning("mnesia timeout waiting for tables: ~p~n", T);
 
warning({mnesia_start_error, M}) ->
    log_warning("mnesia start error: ~p~n", [M]);
 
warning({authentication_failed, M}) ->
    log_warning("authentication failed for: ~p~n", [M]);

warning({authentication_authorization_failed, M}) ->
    log_warning("authentication authorization failed for request: ~p~n", [M]);

warning(X) ->
    log_warning("~p~n", [X]).
 
%%================================================================================
alarm({apply_method_failed, M}) ->
    log_alarm("apply method failed for method: ~p, with arguments: ~p~n", M);

alarm({add_host_failed, M}) ->
    log_alarm("add host: ~p, transaction failed, starting rollback~n", [M]);

alarm({remove_host_failed, M}) ->
    log_alarm("remove host: ~p, transaction failed, starting rollback~n", [M]);

alarm({add_host_and_user_failed, M}) ->
    log_alarm("add host and user transaction failed for host: ~p and user: ~p, starting rollback~n", M);

alarm({remove_host_and_users_failed, M}) ->
    log_alarm("remove host and user transaction failed for host: ~p and user: ~p~n", M);
        
alarm({add_host_user_failed, M}) ->
    log_alarm("add host user failed for user: ~p, failed for host: ~p~n", M);

alarm({remove_host_user_failed, M}) ->
    log_alarm("remove user: ~p, failed for host: ~p~n", M);

alarm({add_user_failed, M}) ->
    log_alarm("add user failed for user: ~p~n", [M]);

alarm({user_registeration_failed, M}) ->
    log_alarm("user registration failed for user: ~p~n", [M]);

alarm({update_user_failed, M}) ->
    log_alarm("update user failed for user: ~p~n", [M]);

alarm({update_client_user_failed, M}) ->
    log_alarm("update client user failed for user: ~p~n", [M]);

alarm({remove_user_failed, M}) ->
    log_alarm("remove user: ~p, failed~n", [M]);

alarm({host_user_deletion_invalid, M}) ->
    log_alarm("user: ~p, does not own host: ~p and tried to delete user~n", M);

alarm({host_and_client_user_database_update_failed, M}) ->
    log_alarm("host and client user database update failed for host: ~p, and user~p~n", M);

alarm({client_user_database_update_failed, M}) ->
    log_alarm("client user database update failed for host: ~p, and user~p~n", M);

alarm({client_user_not_found, M}) ->
    log_alarm("client user not found for host: ~p, and user: ~p~n", M);

alarm({host_access_invalid, M}) ->
    log_alarm("user: ~p, does not own host: ~p and tried to modify it: ~p~n", M);

alarm({admin_authorization_failed, M}) ->
    log_alarm("admin authorization failed for request: ~p from: ~p~n", M);

alarm({host_authorization_failed, M}) ->
    log_alarm("host authorization failed for request: ~p from: ~p~n", M);

alarm({config_load_failed, M}) ->
    log_alarm("config load failed: ~p~n", [M]);

alarm({rollback_failed, M}) ->
    log_alarm("rollback failed for methods:~p with aruments: ~p~n", [M]);

alarm({authentication_error, M}) ->
    log_alarm("authentication database error for: ~p~n", [M]);

alarm(X) ->
    log_alarm("~p~n", [X]).
 
%%================================================================================
log_message(Fmt, Vars) ->
    error_logger:info_msg(Fmt, Vars).
    
%%--------------------------------------------------------------------------------
log_warning(Fmt, Vars) ->
    error_logger:warning_msg(Fmt, Vars).
    
%%--------------------------------------------------------------------------------
log_alarm(Fmt, Vars) ->
    error_logger:error_msg(Fmt, Vars).
 
