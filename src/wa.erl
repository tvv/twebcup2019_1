-module(wa).
-export([
          start/0
        ]).


start() ->
  lists:map(fun appstart/1, applications()).


appstart(App) ->
  case application:start(App) of
    ok -> 
      ok;
    {error, {already_started, App}} -> 
      ok;
    Err -> 
      io:format("{start} Got error ~p on ~p ~n", [Err, App]),
      error
  end.


applications() ->
  [ wa
  ].
