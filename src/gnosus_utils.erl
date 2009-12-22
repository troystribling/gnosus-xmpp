%% gnosus utilities
%%--------------------------------------------------------------------------------
-module(gnosus_utils).

%% API
-export([
    logout/0
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").

%%================================================================================
logout() ->
    #users{uid=Uid} = wf:user(),
    gnosus_logger:message({terminate_session, Uid}),
    wf:clear_user(),
    wf:flash("logged out"),
    wf:redirect("/").
