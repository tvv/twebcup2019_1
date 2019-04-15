-module(page_handler).
-export([
	init/2
	]).

-include("wa.hrl").


init(Req0, State0) ->
	PathInfo = cowboy_req:path_info(Req0),
  Context = #{
    user => authenticate:user(Req0),
    auth => authenticate:is_authenticated(Req0),
    session => session:session(Req0),
    location => location:location(Req0)
  },
  try process(PathInfo, Context, Req0, State0) of
    {ok, Req, State} -> 
      {ok, Req, State}
  catch 
    Ex:Exv:Stacktrace ->
      ?ERROR("Got exception ~p:~p\n~w", [Ex, Exv, Stacktrace]),
      Req = cowboy_req:reply(
      	200,
		    #{<<"content-type">> => <<"text/plain">>},
		    <<"Hello Erlang!">>,
		    Req0),
      {ok, Req, State0}
  end.


process(_PathInfo, _Context, Req0, State) ->
  Req1 = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello Erlang!">>,
    Req0),
  {ok, Req1, State}.