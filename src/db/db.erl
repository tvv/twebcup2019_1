-module(db).
-export([
  migrate/0,
  reset/0,
  sum/1, 
  now/0
]).

-include("wa.hrl").
-include("db.hrl").

-spec migrate() -> list(any()).
migrate() ->
  case applied_migrations() of
    {ok, Applied} ->
      Diff = lists:filter(fun({Prefix, _}) ->
          not(lists:any(fun(A) -> A =:= Prefix end, Applied))
        end, avail_migrations()),
      postprocess(lists:foldl(fun
          ({Prefix, Filename}, ok) ->
            case apply(Filename) of 
              {rollback, Reason} ->
                ?ERROR("Can't apply migration ~p - ~p", [Prefix, Reason]),
                error;
              Result ->
                ?INFO("Migration ~p done with ~p", [Prefix, Result]),
                case sum(Result) of
                  true ->              
                    case db_pg:q(?DBPOOL, "INSERT INTO _migrations(id, filename) VALUES($1, $2);", [Prefix, Filename]) of
                      {ok, _} ->
                        ok;
                      Any ->
                        ?ERROR("Can't fix migration info - ~p", [Any]),
                        error
                    end;
                  false ->
                    error
                end
            end;
          ({Prefix, _}, Any) ->
            ?WARNING("Migration ~p rejected cause previous migrations fail.", [Prefix]),
            Any
        end, ok, sort(Diff)));
    Any ->
      ?ERROR("Can't get applied migrations list - ~p", [Any]),
      Any
  end.

-spec reset() -> ok | {error, any()}.
reset() ->
  case true of %?CONFIG(can_reset_db, false) of
    false ->
      ?ERROR("Can't reset db"),
      {error, not_granted};
    true ->
      case sum(drop_tables()) of
        true ->
          init_migrations(),
          migrate(),
          ok;
        false ->
          {error, can_reset_db}
      end
  end.

-spec now() -> undefined | tuple().
now() ->
  case db_pg:q(?DBPOOL, now_ctime, "SELECT now() at time zone 'utc' AS ctime", []) of
    {ok, [#{ ctime := TS }]} -> 
      TS;
    Error ->
      ?ERROR("Can't get timestamp from server - ~p", [Error]),
      undefined
  end.

-spec sum(list() | tuple()) -> true | false.
sum(List) when is_list(List) ->
  lists:all(fun sum/1, List);
sum({ok, _}) ->
  true;
sum({ok, _, _}) ->
  true;
sum(_) ->
  false.

%
% local
%

postprocess(Any) ->
  Any.

avail_migrations() ->
  Dir = filename:join([wa_app:priv_dir(), "db", "struct"]),
  case file:list_dir(Dir) of
    {ok, Files} ->
      [{prefix(F), filename:join(Dir, F)} || F <- Files];
    Error ->
      ?ERROR("Can't read directory ~p - ~p", [Dir, Error]),
      []
  end.

prefix(Filename) ->
  {P, _} = lists:split(4, Filename),
  P.

applied_migrations() ->
  case db_pg:q(?DBPOOL, "SELECT id FROM _migrations ORDER BY id") of
    {ok, Rows} ->
      {ok, [binary_to_list(R) || #{ id :=R } <- Rows]};
    {error,{error, _, _, undefined_table, _, _}} ->
      init_migrations();
    Any ->
      Any
  end. 

init_migrations() ->
  case {
      db_pg:q(?DBPOOL, 
        "CREATE TABLE _migrations(
            id char(4) NOT NULL,
            filename varchar(256),
            atime timestamp without time zone DEFAULT (now() at time zone 'utc'),
            PRIMARY KEY (id)
          );"),
      db_pg:q(?DBPOOL,     
        "INSERT INTO _migrations(id, filename) VALUES('0000', '0000_create_db.sql');")
    } of 
    {{ok, _}, {ok, _}} ->
      {ok, ["0000"]};
    Any ->
      Any
  end.

sort(Diff) ->
  lists:sort(fun({A,_}, {B, _}) -> 
      list_to_integer(B) > list_to_integer(A) 
    end, Diff).

apply(Filename) ->
  case get_file(filename:extension(Filename), Filename) of
    {ok, Queries} ->
      ?INFO("Processing ~p", [Filename]),
      db_pg:tr(?DBPOOL, fun(C) ->  
          [?PG_SQ(Q)  || Q <- Queries]
        end);
    Error ->
      ?ERROR("Can't get file ~p - ~p", [Filename, Error]),
      error
  end.

get_file(".erl", Filename) ->
  case file:consult(Filename) of
    {ok, [Data]} -> 
      {ok, Data};
    Any -> 
      Any
  end;
get_file(".sql", Filename) ->
  case file:read_file(Filename) of
    {ok, Data} -> 
      {ok, [Data]};
    Any -> 
      Any
  end;  
get_file(_, Filename) ->
  ?ERROR("Unknown extension - ~p", [Filename]),
  {error, unknown_extension}.

drop_tables() ->
  Query = "SELECT table_name
    FROM information_schema.tables
    WHERE table_catalog='stok' AND table_schema='public' AND table_type='BASE TABLE'",
  case db_pg:q(?DBPOOL, Query) of
    {ok, Tables} ->
      Queries = lists:map(fun(#{ table_name := Table }) -> 
          <<"DROP TABLE IF EXISTS ", Table/binary, " CASCADE">> 
        end, Tables),
      db_pg:tr(?DBPOOL, fun(C) ->  
          [?PG_SQ(Q)  || Q <- Queries]
        end);
    Any ->
      Any
  end.
