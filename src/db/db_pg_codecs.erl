   -module(db_pg_codecs).
-behaviour(epgsql_codec).

-export([
  init/2, 
  names/0,
  encode/3, 
  decode/3
]).

-include("wa.hrl").

-define(JSONB_VERSION_1, 1).


init(State, _C) ->
  State.

names() ->
  [json, jsonb].

encode(Data, json, _State) when is_map(Data) ->
  try ?TO_JSON(Data) of
    EData -> 
      EData
  catch 
    Ex:Exp:_Stacktrace ->
      ?ERROR("Can't encode JSON ~p - ~p:~p", [Data, Ex, Exp]),
      Data
  end;
encode(Data, jsonb, _State) when is_map(Data) ->
  try ?TO_JSON(Data) of
    EData -> 
      [<<?JSONB_VERSION_1:8>> | EData]
  catch 
    Ex:Exp:_Stacktrace ->
      ?ERROR("Can't encode JSON ~p - ~p:~p", [Data, Ex, Exp]),
      Data
  end;
encode(Data, jsonb, _State) ->
    [<<?JSONB_VERSION_1:8>> | Data];
encode(Data, _Type, _State) ->
  Data.  

decode(Data, json, _State) ->
  try ?FROM_JSON(Data) of
    DData -> 
      DData
  catch 
    Ex:Exp:_Stacktrace ->
      ?ERROR("Can't decode JSON ~p - ~p:~p", [Data, Ex, Exp]),
      Data
  end;
decode(<<?JSONB_VERSION_1:8, Data/binary>>, jsonb, _State) ->
  try ?FROM_JSON(Data) of
    DData -> 
      DData
  catch 
    Ex:Exp:_Stacktrace ->
      ?ERROR("Can't decode JSON ~p - ~p:~p", [Data, Ex, Exp]),
      Data
  end;
decode(Data, _Type, _State) ->
  Data.
