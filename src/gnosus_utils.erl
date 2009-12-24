%% gnosus utilities
%%--------------------------------------------------------------------------------
-module(gnosus_utils).

%% API
-export([
    logout/0,
    host_page_redirect/0,
    start_page_redirect/0
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").

%%================================================================================
logout() ->
    #users{uid=Uid} = wf:user(),
    gnosus_logger:message({terminate_session, Uid}),
    wf:logout(),
    wf:flash("logged out"),
    wf:redirect("/").

%%--------------------------------------------------------------------------------
host_page_redirect() ->
    User = wf:user(),
    case User#users.product of
        unlimited ->
            wf:redirect("/web/hosts");
        _ ->
            Host = hd(wf:session(hosts)),
            wf:redirect("/web/host/"++Host)
    end.

%%--------------------------------------------------------------------------------
start_page_redirect() ->
    case length(wf:session(hosts)) of
        0 ->
            wf:redirect("/web/host/create");
        _ ->
            host_page_redirect()
    end.
        
    
