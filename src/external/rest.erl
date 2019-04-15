-module(rest).

-export([
  request/6
]).

-include("wa.hrl").
-include("external.hrl").


request(Method, Type, URL, Expect, InHeaders, Body) ->
  try request_throwable(Method, Type, URL, Expect, InHeaders, Body) of
    Any -> Any
  catch
    Type:Reason:_Stacktrace  ->
      ?ERROR("Exception catched ~p:~p. ~p", [Type, Reason, [Method, Type, URL, Expect, InHeaders]]),
      {error, exception}
  end.

request_throwable(Method, Type, URL, Expect, InHeaders, Body) when is_binary(URL) ->
  request_throwable(Method, Type, binary_to_list(URL), Expect, InHeaders, Body);
request_throwable(Method, Type, URL, Expect, InHeaders, Body) ->
  Headers = maps:merge(InHeaders, #{
      <<"Accept">> => get_access_type(Type) ++ ", */*;q=0.9",
      <<"Content-Type">> => get_content_type(Type)
    }),
  case http_uri:parse(URL) of
    {ok, {_Scheme, _, Host, Port, Path, QS}} ->
      case connect(Host, Port) of
        {ok, ConnPid} ->
          {ok, StreamRef} = do_request(ConnPid, Method, Type, Path, QS, Headers, Body),
          Response = get_response(ConnPid, StreamRef, Expect),
          gun:shutdown(ConnPid),
          Response;
        Any ->
          Any
      end;
    Any ->
      Any
  end.

connect(Host, Port) ->
  case gun:open(Host, Port) of
    {ok, ConnPid} ->
      case gun:await_up(ConnPid, ?GUN_TIMEOUT) of
        {ok, _} ->
          {ok, ConnPid};
        Any ->
          Any
      end;
    Any ->
      Any
  end.

do_request(ConnPid, Method, Type, Path, QS, Headers, Body) when Method =:= post; Method =:= put -> 
  EncBody = encode_body(Type, Body),
  {ok, gun:Method(
    ConnPid, Path ++ QS, maps:to_list(Headers#{ <<"content-length">> => byte_size(EncBody) }), EncBody)};
do_request(ConnPid, Method, _Type, Path, QS, Headers, Body) when length(QS) =:= 0 ->
  {ok, gun:Method(
    ConnPid, lists:concat([Path, "?", binary_to_list(encode_body(qs, Body))]), maps:to_list(Headers))};
do_request(ConnPid, Method, _Type, Path, QS, Headers, Body) when length(QS) =/= 0 ->
  {ok, gun:Method(
    ConnPid, lists:concat([Path, QS, "&", binary_to_list(encode_body(qs, Body))]), maps:to_list(Headers))};
do_request(ConnPid, Method, _Type, Path, QS, Headers, _Body) ->
  {ok, gun:Method(ConnPid, Path ++ QS, maps:to_list(Headers))}.

get_response(ConnPid, StreamRef, Expect) ->
  case gun:await(ConnPid, StreamRef) of
    {response, fin, Status, RespHeaders} ->
      case lists:any(fun(I) -> I =:= Status end, Expect) of
        true ->
          {ok, Status, RespHeaders, #{}};
        false when Expect =:= [] ->
          {ok, Status, RespHeaders, #{}};
        false ->
          {error, Status, RespHeaders, #{}}
      end;
    {response, nofin, Status, RespHeadersL} ->
      RespHeaders = maps:from_list([{string:lowercase(K), V} || {K, V} <- RespHeadersL]),
      case gun:await_body(ConnPid, StreamRef) of
        {ok, RespBody} ->
          case lists:any(fun(I) -> I =:= Status end, Expect) of
            true ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false when Expect =:= [] ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false ->
              {error, Status, RespHeaders, parse(RespHeaders, RespBody)}
          end;
        Error ->
          {error, Error}
      end;
    Any -> Any
  end.

get_access_type(html) -> 
  "text/html";
get_access_type(qs) -> 
  "application/x-www-form-urlencoded";
get_access_type(_) -> 
  "application/json".

get_content_type(html) -> 
  "text/html";
get_content_type(qs) -> 
  "application/x-www-form-urlencoded";
get_content_type(binary) -> 
  "application/binary";
get_content_type(_) -> 
  "application/json".

encode_body(qs, Body) -> 
  cow_qs:qs(maps:to_list(Body));
encode_body(html, Body) -> 
  Body;
encode_body(binary, Body) -> 
  Body;
encode_body(_, Body) -> 
  ?TO_JSON(Body).

parse(Headers, Body) ->
  Unzip = case maps:get(<<"content-encoding">>, Headers, <<"plain">>) of
    <<"gzip">> -> 
      zlib:gunzip(Body);
    _ -> 
      Body
  end,
  case maps:get(<<"content-type">>, Headers, undefined) of
    undefined -> 
      parse_body("application/json", Unzip);
    Type -> 
      CType = hd(string:tokens(binary_to_list(Type), ";")),
      parse_body(CType, Unzip)
  end.

parse_body(_, <<>>) -> 
  undefined; 
parse_body("application/json", Body) -> 
  ?FROM_JSON(Body); 
parse_body("text/javascript", Body) -> 
  ?FROM_JSON(Body); 
parse_body("application/x-www-form-urlencoded", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body));
parse_body("text/plain", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body)); % damn you, Facebook
parse_body(_, Body) -> 
  Body.
