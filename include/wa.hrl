%
% Common project options
%

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 50, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 50, Type, [I]}).
-define(CHILD(Id, I, Type, Param), {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

-define(FROM_JSON(D), jiffy:decode(D, [return_maps])).
-define(TO_JSON(D), jiffy:encode(D)).

%
% Configuration
%

-define(CONFIG(Key, Default), application:get_env(wa, Key, Default)).
-define(PV(Key, Set), proplists:get_value(Key, Set)).
-define(PV(Key, Set, Default), proplists:get_value(Key, Set, Default)).

%
% Logger
%

-define(LOG(Level, Msg, Params), logger:Level("[~p:~p] " Msg, [?MODULE, ?LINE] ++ Params)).
-define(ERROR(Msg), ?LOG(error, Msg, [])).
-define(ERROR(Msg, Params), ?LOG(error, Msg, Params)).
-define(INFO(Msg), ?LOG(info, Msg, [])).
-define(INFO(Msg, Params), ?LOG(info, Msg, Params)).
-define(WARNING(Msg), ?LOG(warning, Msg, [])).
-define(WARNING(Msg, Params), ?LOG(warning, Msg, Params)).
-define(DEBUG(Msg), ?LOG(debug, Msg, [])).
-define(DEBUG(Msg, Params), ?LOG(debug, Msg, Params)).

%
% Pub/Sub
%

% -define(ME(Reg), gproc:reg({n, l, Reg})).
% -define(LOOKUP(Reg), iomod:lookup(Reg)).
% -define(LOOKUPS(Reg), iomod:lookups(Reg)).
% -define(PUB(Event, Msg), iomod:pub(Event, Msg)).
% -define(SUB(Event), iomod:sub(Event)).
% -define(UNSUB(Event), iomod:unsub(Event)).
% -define(LOOKUP_SUB(Reg), gproc:lookup_pids({p, l, Reg})).
% -define(IS_SUB(Event), lists:any(fun({P, _}) -> P =:= self() end, gproc:lookup_local_properties(Event))).

%
% Users
%

-define(S2MS(S), S * 1000).


-define(RE_MAIL, "^[^@<> ]+@[^@<> ]+\\.[^@<> ]{2,}$").

-define(RECONNECT_TIMEOUT, ?S2MS(5)).
-define(WS_TIMEOUT, 24 * 60 * 60). % 24h
-define(MAX_CHAT_HISTORY, 40).

-define(DEFAULT_LOCATION, 79).
-define(DEFAULT_LOCATION_OKATO, <<"45000000000">>).

-define(CAPTCHA_ABC, "qwertyupafhkxvnmQWERYUPAFHKXVNM3479"). 
-define(PWD_ABC, "qwertyupasdfghkzxcvbnmQWERTYUPASDFGHKZXCVLNM2345679").
-define(TOKEN_ABC, "qwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNM"). 

-define(OK, #{ result => ok, can => ok }).
-define(OK(Can), #{ result => ok, can => Can }).
-define(ER(Error), #{ result => error, error => Error, can => ok }).
-define(ER(Error, Can), #{ result => error, error => Error, can => Can }).
-define(VALIDATION(Errors), #{ result => error, validation => Errors, can => ok }).
-define(VALIDATION(Errors, Can), #{ result => error, validation => Errors, can => Can }).

%
% Shortcuts
%

-define(IF(Action, Then, Else), if Action =:= true -> Then; true -> Else end).

-define(CACHE(Section, Name, Fun, Default), 
  case db_redis:q(cache, ["HGET", Section, Name]) of
    {ok, Val} when Val =/= undefined ->
      binary_to_term(Val);
    _Any ->
      case Fun() of
        {ok, NewVal} ->
          db_redis:q(cache, ["HSET", Section, Name, term_to_binary(NewVal)]),
          NewVal;
        Error ->
          ?ERROR("Can't put data to cache ~p - ~p", [Name, Error]),
          Default
      end
  end
).
-define(CACHEV(Name, TTL, Fun, Default), 
  case db_redis:q(cache, ["GET", Name]) of
    {ok, Val} when Val =/= undefined ->
      binary_to_term(Val);
    _Any ->
      case Fun() of
        {ok, NewVal} ->
          db_redis:q(cache, ["SET", Name, term_to_binary(NewVal), "EX", TTL]),
          NewVal;
        Error ->
          ?ERROR("Can't put data to cache ~p - ~p", [Name, Error]),
          Default
      end
  end
).
