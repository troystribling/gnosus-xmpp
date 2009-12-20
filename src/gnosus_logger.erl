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

message({domain_created, M}) ->
    error_logger:info_msg("domain: ~p, created for user: ~p~n", M);

message({add_domain_succeeded, M}) ->
    error_logger:info_msg("add domain: ~p, transaction succeeded at ~p~n", M);

message({remove_domain_succeeded, M}) ->
    error_logger:info_msg("remove domain: ~p, transaction succeeded at ~p~n", M);

message({add_domain_and_user_succeeded, M}) ->
    error_logger:info_msg("add domain and user transaction succeeded for domain: ~p and user: ~p at ~p~n", M);

message({remove_domain_and_users_succeeded, M}) ->
    error_logger:info_msg("remove domain and users transaction succeeded for domain: ~p at ~p~n", M);

message({domain_deleted, M}) ->
    error_logger:info_msg("domain: ~p, deleted for user: ~p~n", M);

message({domain_user_deleted, M}) ->
    error_logger:info_msg("user: ~p, deleted for domain: ~p~n", M);

message({domain_user_created, M}) ->
    error_logger:info_msg("user: ~p, created for domain: ~p~n", M);

message({user_created, M}) ->
    error_logger:info_msg("user: ~p, created", [M]);

message({user_deleted, M}) ->
    error_logger:info_msg("user: ~p, deleted", [M]);

message({registered_user, M}) ->
    error_logger:info_msg("registered user: ~p~n", [M]);

message({config_loaded, M}) ->
    error_logger:info_msg("configuration loaded: ~p~n", [M]);

message(X) ->
    error_logger:info_msg("~p~n", [X]).
 
%%--------------------------------------------------------------------------------
warning({mnesia_start_timeout, T}) ->
    error_logger:warning_msg("mnesia timeout waiting for tables: ~p~n", T);
 
warning({mnesia_start_error, M}) ->
    error_logger:warning_msg("mnesia start error: ~p~n", [M]);
 
warning({authentication_failed, M}) ->
    error_logger:info_msg("authentication failed for: ~p~n", [M]);

warning({authentication_authorization_failed, M}) ->
    error_logger:info_msg("authentication authorization failed for request: ~p~n", [M]);

warning(X) ->
    error_logger:warning_msg("~p~n", [X]).
 
%%--------------------------------------------------------------------------------
alarm({domain_deletion_failed, M}) ->
    error_logger:error_msg("domain: ~p, deletion failed for user: ~p~n", M);

alarm({domain_creation_failed, M}) ->
    error_logger:error_msg("domain: ~p, creation failed for user: ~p~n", M);

alarm({add_domain_failed, M}) ->
    error_logger:error_msg("add domain: ~p, transaction failed at ~p~n", M);

alarm({remove_domain_failed, M}) ->
    error_logger:error_msg("remove domain: ~p, transaction failed at ~p~n", M);

alarm({add_domain_and_user_failed, M}) ->
    error_logger:error_msg("add domain and user transaction failed for domain: ~p and user: ~p at ~p~n", M);

alarm({remove_domain_and_users_failed, M}) ->
    error_logger:error_msg("remove domain and users transaction failed for domain: ~p at ~p~n", M);

alarm({domain_user_creation_failed, M}) ->
    error_logger:error_msg("user: ~p, creation failed for domain: ~p~n", M);

alarm({domain_user_deletion_failed, M}) ->
    error_logger:error_msg("user: ~p, deletion failed for domain: ~p~n", M);

alarm({user_creation_failed, M}) ->
    error_logger:error_msg("user: ~p, creation failed", [M]);

alarm({user_deletion_failed, M}) ->
    error_logger:error_msg("user: ~p, deletion failed", [M]);

alarm({domain_user_deletion_invalid, M}) ->
    error_logger:error_msg("user: ~p, does not own domain: ~p and tried to delete user~n", M);

alarm({domain_user_creation_invalid, M}) ->
    error_logger:error_msg("user: ~p, does not own domain: ~p and tried to create user~n", M);

alarm({domain_deletion_invalid, M}) ->
    error_logger:error_msg("user: ~p, does not own domain: ~p and tried to delete it~n", M);

alarm({domain_show_invalid, M}) ->
    error_logger:error_msg("user: ~p, does not own domain: ~p and tried to show it~n", M);

alarm({admin_authorization_failed, M}) ->
    error_logger:error_msg("admin authorization failed for request: ~p from: ~p~n", M);

alarm({config_load_failed, M}) ->
    error_logger:error_msg("error loading config: ~p~n", [M]);

alarm(X) ->
    error_logger:error_msg("~p~n", [X]).
 
 
