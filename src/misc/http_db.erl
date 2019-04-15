-module(http_db).
-export([
  search_query/2,
  model_create_query/2,
  delete_query/2
  ]).

-include("wa.hrl").
-include("http_api.hrl").



-spec delete_query(list(), integer()) -> {list(), []} | {error, term()}.
delete_query(Table, Id) ->
  {"DELETE FROM " ++ Table ++ " WHERE id=$1", [Id]}.


-spec model_create_query(list(), #{}) -> {list(), []} | {error, term()}.
model_create_query(Table, Model) ->
  {TableQuery, ValsQuery, Vals, _} = maps:fold(fun(MapKey, MapValue, {TableQuery, ValsQuery, MapValues, MapIdx}) ->
    {TableQuery ++ utils:to_list(MapKey) ++ ", ", ValsQuery ++ "$" ++ utils:to_list(MapIdx) ++ ", ", MapValues ++ [MapValue], MapIdx+1}
  end, {"", "", [], 1}, Model),
  SlicedTableQuery = string:slice(TableQuery, 0, string:length(TableQuery)-2),
  {
    "INSERT INTO " ++ Table ++ "(" ++ SlicedTableQuery ++ ") " ++  
      "VALUES(" ++ string:slice(ValsQuery, 0, string:length(ValsQuery)-2) ++ ");", 
    Vals
  }.


-spec search_query(list(), http_api_request()) -> {ok, {list(), list()}} | {error, term()}.
search_query(Table, #http_api_request{ return_fields = RetFields, filters = Filters, order = Order, limit = Limit, offset = Offset }) ->
  RetFieldQ = build_return_fields_query(RetFields),
  ?INFO("Filters: ~p", [Filters]),
  {WhereQ, WhereVals} = build_where_query(Filters),
  OrderQ = build_order_query(Order),
  LimitOffsetQ = build_limit_offset_query(Limit, Offset),
  { 
    "SELECT " ++ RetFieldQ ++ " FROM " ++ Table ++ " " ++ 
      WhereQ ++ " " ++ 
      OrderQ ++ " " ++ 
      LimitOffsetQ ++ ";",
    WhereVals
  }.


-spec build_limit_offset_query(integer(), integer() | undefined) -> list().
build_limit_offset_query(undefined, undefined) ->
  "";
build_limit_offset_query(Limit, undefined) when is_integer(Limit)->
  "LIMIT " ++ utils:to_list(Limit);
build_limit_offset_query(Limit, Offset) when is_integer(Limit), is_integer(Offset) ->
  "LIMIT " ++ utils:to_list(Limit) ++ " OFFSET " ++ utils:to_list(Offset);
build_limit_offset_query(_, _) ->
  "".



-spec build_order_query([{atom(), atom()}]) -> list().
build_order_query(undefined) ->
  "";
build_order_query([]) ->
  "";
build_order_query([{_, _}|_] = L) ->
  Q = lists:foldl(fun({Field, OrderType}, A) ->
    A ++ atom_to_list(Field) ++ " " ++ case OrderType of
      asc -> "ASC";
      desc -> "DESC"
    end ++ ", "
  end, "", L),
  "ORDER BY " ++ string:slice(Q, 0, string:length(Q) - 2);
build_order_query(_) ->
  "".


-spec build_where_query(#{atom() => [{atom(), any()}]}) -> {list(), list()}.
build_where_query(FiltersMap) when map_size(FiltersMap) == 0 ->
  {"", []};
build_where_query(FiltersMap) ->
  {Q, Vals, _} = maps:fold(fun(K, V, {MQ, MV, MIdx}) ->
    {LQ, LV, LIdx} = lists:foldl(fun({Op, Val}, {AccL, Vals, LIdx}) ->
      {FQ, FV, NextIdx} = filter_op_to_list(atom_to_list(K), Op, Val, LIdx),
      {AccL ++ FQ ++ " AND ", Vals ++ FV, NextIdx}
    end, {"", MV, MIdx}, V),
    {MQ ++ LQ, LV, LIdx}
  end, {"", [], 1}, FiltersMap),
  {"WHERE " ++ string:slice(Q, 0, string:length(Q) - 5), Vals}.


-spec filter_op_to_list(list(), atom(), any(), integer()) -> {list(), list(), integer()} | {error, term()}.
filter_op_to_list(Field, Op, Value, Idx) ->
  StrIdx = utils:to_list(Idx),
  case Op of
    eq -> 
      {Field ++ " = $" ++ StrIdx, [Value], Idx+1};
    neq -> 
      {Field ++ " != $" ++ StrIdx, [Value], Idx+1};
    gt -> 
      {Field ++ " > $" ++ StrIdx, [Value], Idx+1};
    gte -> 
      {Field ++ " >= $" ++ StrIdx, [Value], Idx+1};
    lt -> 
      {Field ++ " < $" ++ StrIdx, [Value], Idx+1};
    lte -> 
      {Field ++ " <= $" ++ StrIdx, [Value], Idx+1};
    like -> 
      {Field ++ " LIKE $" ++ StrIdx, [Value], Idx+1};
    ilike -> 
      {Field ++ " ILIKE $" ++ StrIdx, [Value], Idx+1};
    in -> 
      {Q, Vals, NextIdx} = prepare_array(Value, Idx),
      {Field ++ " IN (" ++ Q ++ ")", Vals, NextIdx};
    nin -> 
      {Q, Vals, NextIdx} = prepare_array(Value, Idx),
      {Field ++ " NOT IN (" ++ Q ++ ")", Vals, NextIdx};
    _ -> {error, wrong_value}
  end.


-spec prepare_array([], integer()) -> {list(), [], integer()}.
prepare_array(L, Idx) ->
  {Q, Vals, NextIdx} = lists:foldl(fun(I, {A, Vals, NextIdx}) ->
    {A ++ "$" ++ utils:to_list(NextIdx) ++ ", ", Vals ++ [I], NextIdx+1}
  end, {"", [], Idx}, L),
  {string:slice(Q, 0, string:length(Q) - 2), Vals, NextIdx}.


-spec concat([], list()) -> list().
concat(L, S) ->
  R = lists:foldl(fun(I,A) -> A++utils:to_list(I)++S end, "", L),
  string:slice(R, 0, string:length(R)-string:length(S)).


-spec build_return_fields_query([atom()]) -> list().
build_return_fields_query(ReturnFields) ->
  concat(ReturnFields, ", ").