-module(http_handler).
-export([
  redirect/2,
  not_implemented/1,
  notauthenticated_json_response/1,
  errormessage_json_response/2,
  notvalid_json_response/2,
  success_json_response/2,
  error_json_response/3,
  json_response/3,
  context/1,
	handle/3,
  auth_required/3
	]).

-include("wa.hrl").
-include("session.hrl").


redirect(Uri, Req0) ->
  cowboy_req:reply(301,
    #{<<"location">> => Uri},
    <<"">>,
    Req0).


not_implemented(Req0) ->
  cowboy_req:reply(501,
    #{<<"content-type">> => <<"plain/text">>},
    <<"Not implemented">>,
    Req0).


success_json_response(Data, Req0) ->
  json_response(
    200, #{
    success => true,
    data => Data
    }, Req0).


notauthenticated_json_response(Req0) ->
  error_json_response(401, #{}, Req0).


notvalid_json_response(Errors, Req0) ->
  error_json_response(400, #{errors => Errors}, Req0).


errormessage_json_response(Message, Req0) ->
  error_json_response(400, #{message => Message}, Req0).


error_json_response(Status, Data, Req0) ->
  json_response(Status, Data#{success => false}, Req0).


json_response(Status, Data, Req0) ->
  cowboy_req:reply(Status,
    #{<<"content-type">> => <<"application/json">>},
    iomod:out(Data),
    Req0).


-spec context(cowboy_req:req()) -> #{user => map(), is_authenticated => boolean(), session => session() | undefined}.
context(Req0) ->
  #{
    user             => authenticate:user(Req0),
    is_authenticated => authenticate:is_authenticated(Req0),
    session          => session:session(Req0)
  }.


handle(Handler, Req0, State0) ->
  PathInfo = cowboy_req:path_info(Req0),
  try Handler(PathInfo, Req0, State0) of
    {ok, Req, State} -> 
      {ok, Req, State}
  catch 
    Ex:Exv:Stacktrace ->
      ?ERROR("Got exception ~p:~p\n~p", [Ex, Exv, Stacktrace]),
      Req = cowboy_req:reply(
      	500,
		    #{<<"content-type">> => <<"text/plain">>},
		    <<>>,
		    Req0),
      {ok, Req, State0}
  end.


auth_required(Handler, Req0, State0) ->
  case authenticate:user(Req0) of
    undefined -> 
      Req1 = http_handler:notauthenticated_json_response(Req0),
      {ok, Req1, State0};

    User ->
      case erlang:fun_info(Handler, arity) of    
        {arity, 1} ->
          Handler(User);

        _ ->
          Handler()
      end
  end.