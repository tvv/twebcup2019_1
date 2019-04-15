-module(authenticate_middleware).
-export([
	execute/2
	]).

-include("wa.hrl").
-include("session.hrl").
-include("authenticate.hrl").


-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req0, Env) ->
	Req = auth_users(Req0), 
	{ok, Req, Env}.


-spec auth_users(Req) -> Req
	when Req::cowboy_req:req().
auth_users(Req0) ->
	auth_users(Req0, [public, backoffice]).


-spec auth_users(Req, Types) -> Req
	when Req::cowboy_req:req(), Types::[session_type()].
auth_users(Req0, []) ->
	Req0;
auth_users(Req0, [Type|Rest]) ->
	auth_users(authenticate:auth(Req0, Type), Rest).