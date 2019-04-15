-module(utils).
-export([
  concat/2,

  to_integer/1,
  to_integer/2,
  to_float/1,
  to_float/2,
  to_binary/1,
  to_list/1,
  format_date/1,
  date_to_timestamp/1,
  timestamp_to_date/1,
  parse_date/1,

  map_to_json/1,
  list_to_json/1
]).

-include("wa.hrl").
-include("db.hrl").

-spec concat([binary()], binary()) -> binary().
concat([], _) -> 
  <<>>;
concat([E], _) -> 
  E;
concat([A1, A2 | T], Del) -> 
  concat([<<(to_binary(A1))/binary, Del/binary, (to_binary(A2))/binary>> | T], Del).

%
% Types
%

-spec to_integer(any()) -> integer().
to_integer(Val) -> 
  to_integer(Val, 0).

-spec to_integer(any(), integer()) -> integer().
to_integer(Int, _) when is_integer(Int) ->
  Int;
to_integer(IntB, Default) when is_binary(IntB) ->
  to_integer(IntB, fun binary_to_integer/1, Default);
to_integer(IntL, Default) when is_list(IntL) ->
  to_integer(IntL, fun list_to_integer/1, Default);
to_integer(_, Default) -> 
  Default.

to_integer(IntT, Conv, Default) ->
  try Conv(IntT) of
    Int -> Int
  catch _:_:_ -> 
    Default
  end.

-spec to_float(any()) -> float().
to_float(Val) -> 
  to_float(Val, 0.0).

-spec to_float(any(), float()) -> float().
to_float(Float, _) when is_float(Float) ->
  Float;
to_float(Float, _) when is_integer(Float) ->
  float(Float);
to_float(FloatB, Default) when is_binary(FloatB) ->
  to_float(FloatB, fun binary_to_float/1, Default);
to_float(FloatL, Default) when is_list(FloatL) ->
  to_float(FloatL, fun list_to_float/1, Default);
to_float(_, Default) -> Default.

to_float(FloatT, Conv, Default) ->
  try Conv(FloatT) of
    Float -> Float
  catch 
    error:badarg:_ -> 
      float(to_integer(FloatT, Default));
    _:_:_ -> 
      Default
  end.

-spec to_binary(any()) -> binary().
to_binary(V) when is_binary(V) -> 
  V;
to_binary(V) when is_list(V) -> 
  unicode:characters_to_binary(V);
to_binary(V) when is_integer(V) -> 
  integer_to_binary(V);
to_binary(V) when is_float(V) -> 
  float_to_binary(V, [compact]);
to_binary(V) when is_atom(V) -> 
  atom_to_binary(V, utf8);
to_binary(true) -> 
  <<"TRUE">>;
to_binary(false) -> 
  <<"FALSE">>.

-spec to_list(any()) -> list().
to_list(V) when is_list(V) -> 
  V;
to_list(V) when is_binary(V) -> 
  unicode:characters_to_list(V);
to_list(V) when is_integer(V) -> 
  integer_to_list(V);
to_list(V) when is_float(V) -> 
  io_lib:format("~.2f",[V]);
to_list(V) when is_atom(V) -> 
  atom_to_list(V);
to_list(true) -> 
  "TRUE";
to_list(false) -> 
  "FALSE".

format_date(DateTime) -> 
  iso8601:format(DateTime).

date_to_timestamp({Date, {H, M, S}}) ->
  RS = floor(S),
  calendar:datetime_to_gregorian_seconds({Date, {H, M, RS}}) - ?EPOCH + (S - RS).

timestamp_to_date(TS) ->
  RTS = floor(TS),
  {Date, {H, M, RS}} = calendar:gregorian_seconds_to_datetime(RTS + ?EPOCH),
  {Date, {H, M, RS + (TS - RTS)}}.

parse_date({{_, _, _}, {_, _, _}} = DateTime) ->
  DateTime;
parse_date(<<Y:4/binary, X, M:2/binary, X, D:2/binary>>) ->
  {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}, {0,0,0}};
parse_date(Str) when is_list(Str) -> 
  parse_date(list_to_binary(Str));
parse_date(Str) ->
  case re:run(Str, "([0-9]{1,4})-([01][0-9])-([0-3][0-9])T([0-1][0-9]):([0-5][0-9]):([0-5][0-9].[0-9]+)Z", [global, {capture, all, binary}]) of
    {match, [[_, Year, Month, Day, Hour, Minute, Seconds]]} ->
      { {to_integer(Year), to_integer(Month), to_integer(Day)},
        {to_integer(Hour), to_integer(Minute), to_float(Seconds)}};
    _ ->
      iso8601:parse(Str)
  end.

%
% Maps
%

-spec map_to_json(map()) -> map().
map_to_json(M) ->
  maps:map(fun
      (_K, V) when is_map(V) -> 
        map_to_json(V);
      (_K, {{_, _, _}, {_, _, _}} = V) -> 
        format_date(V);
      (_K, V) when is_tuple(V) -> 
        list_to_json(tuple_to_list(V));
      (_K, V) when is_list(V) -> 
        list_to_json(V);
      (_K, V) -> 
        V
    end, M).

-spec list_to_json(list()) -> list().
list_to_json(L) ->
  lists:map(fun
      (I) when is_map(I) -> 
        map_to_json(I);
      ({{_, _, _}, {_, _, _}} = I) -> 
        format_date(I);
      (I) when is_tuple(I) -> 
        tuple_to_list(I);
      (I) -> 
        I 
    end, L).
