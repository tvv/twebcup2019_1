-module(db_table).
-export([
	new/3,
	get/3,
	get/4,
	find/3,
	find/4,
	update/4,
	delete/3
	]).

-include("wa.hrl").


-spec new(list(), #{atom() => any()}, list()) -> {ok, #{atom() => any()}} | {error, term()}.
new(Table, Record, Returning) ->
	{_, Fields, Placeholders, Params} =
		maps:fold(
			fun
				(F, V, {1, _, _, _}) -> 
					{2, [atom_to_list(F)], ["$1"], [V]};
				(F, V, {Idx, Fs, Ps, Vs}) -> 
					{Idx + 1, Fs ++ ", " ++ atom_to_list(F), Ps ++ ", $" ++ integer_to_list(Idx), Vs ++ [V]}
			end, 
			{1, [], [], []}, Record),
  Query = "INSERT INTO " ++ Table ++ " (" ++ Fields ++ ") VALUES (" ++ Placeholders ++ ") RETURNING " ++ Returning,
  case db_pg:q(pgdb, list_to_atom("db_table_new_" ++ Table), Query, Params) of
    {ok, 1, [Row]} -> {ok, Row};    
    {ok, _, []}    -> {error, not_created};
    Any            -> Any
  end.


-spec get(list(), integer(), list()) -> {ok, #{atom() => any()}} | {error, term()}.
get(Table, Id, Returning) ->
	get(pgdb, Table, Id, Returning).


 -spec get(atom(), list(), integer(), list()) -> {ok, #{atom() => any()}} | {error, term()}.
get(Pool, Table, Id, Returning) ->
  Query = "SELECT " ++ Returning ++ " FROM " ++ Table ++ " WHERE id=$1",
  Params = [Id],
  case db_pg:q(Pool, list_to_atom("db_table_get_" ++ Table), Query, Params) of
    {ok, [Row]}  -> {ok, Row};
    {ok, []}     -> {error, not_found};
    Any          -> Any
  end.


-spec find(atom(), list(), list()) -> {ok, [#{atom() => any()}]} | {error, term()}.
find(QueryId, Query, Params) ->
	find(pgdb, QueryId, Query, Params).


-spec find(atom(), atom(), list(), list()) -> {ok, [#{atom() => any()}]} | {error, term()}.
find(Pool, QueryId, Query, Params) ->
  db_pg:q(Pool, QueryId, Query, Params).


-spec update(list(), integer(), #{atom() => any()}, list()) -> {ok, #{atom() => any()}} | {error, term()}.
update(Table, Id, Record, Returning) ->
	{_, Fields, Params} =
		maps:fold(
			fun
				(F, V, {2, _, _}) -> 
					{3, [atom_to_list(F) ++ "=$2"], [V]};
				(F, V, {Idx, Fs, Vs}) -> 
					{Idx + 1, Fs ++ ", " ++ atom_to_list(F) ++ "=$" ++ integer_to_list(Idx), [V | Vs]}
			end, 
			{2, [], [Id]}, Record),
  Query = "UPDATE " ++ Table ++ " SET " ++ Fields ++ ", atime=(now() at time zone 'utc') WHERE id=$1 RETURNING " ++ Returning,
  case db_pg:q(pgdb, list_to_atom("db_table_update_" ++ Table), Query, Params) of
    {ok, [Row]}  -> {ok, Row};
    {ok, []}     -> {error, not_found};
    Any          -> Any
  end.

-spec delete(list(), integer(), list()) -> {ok, #{atom() => any()}} | {error, term()}.
delete(Table, Id, Returning) ->
  Query = "UPDATE " ++ Table ++ " SET removed=TRUE, dtime=(now() at time zone 'utc') WHERE id=$1 RETURNING " ++ Returning,
  Params = [Id],
  case db_pg:q(pgdb, list_to_atom("db_table_delete_" ++ Table), Query, Params) of
    {ok, [Row]}  -> {ok, Row};
    {ok, []}     -> {error, not_found};
    Any          -> Any
  end.