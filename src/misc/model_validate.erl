-module(model_validate).
-export([
  validate/3,
  validate/4,
  v_required/2,
  v_optional/2,
  v_min/2,
  v_max/2,
  v_minmax/2,
  v_enum/2,
  v_model/2,
  v_regexp/2,
  v_key/1,
  v_clean_string/2,
  v_trim_string/1,
  v_escape_string/2,
  v_path/1,
  v_login/1,
  v_json/1,
  v_entity_exists/2,
  v_entity_not_exists/2
  ]).

-include("wa.hrl").
-include("models.hrl").
-include("db.hrl").
-define(NOT_SET, '_not_set_').


-spec validate(any(), #{atom() => any()}, atom()) -> ok | {ok, any()} | {error, term()}.
validate(V0, D, Type) ->
  validate(V0, D, Type, ?NOT_SET).

-spec validate(any(), #{atom() => any()}, atom(), any()) -> ok | {ok, any()} | {error, term()}.
validate(V0, _D, required, Opts) ->
  v_required(V0, Opts);
validate(V0, _D, optional, Opts) ->
  v_optional(V0, Opts);
validate(V0, _D, min, Opts) ->
  v_min(V0, Opts);
validate(V0, _D, max, Opts) ->
  v_max(V0, Opts);
validate(V0, _D, minmax, Opts) ->
  v_minmax(V0, Opts);
validate(V0, _D, enum, Opts) ->
  v_enum(V0, Opts);
validate(V0, _D, model, Opts) ->
  v_model(V0, Opts);
validate(V0, _D, regexp, Opts) ->
  v_regexp(V0, Opts);
validate(V0, _D, key, _Opts) ->
  v_key(V0);
validate(V0, _D, clean, text) ->
  v_clean_string(V0, text);
validate(V0, _D, clean, string) ->
  v_clean_string(V0, string);
validate(V0, _D, trim, _Opts) ->
  v_trim_string(V0);
validate(V0, _D, escape, text) ->
  v_escape_string(V0, text);
validate(V0, _D, escape, string) ->
  v_escape_string(V0, string);
validate(V0, _D, path, _Opts) ->
  v_path(V0);
validate(V0, _D, login, _Opts) ->
  v_login(V0);
validate(V0, _D, json, _Opts) ->
  v_json(V0);
validate(V0, _D, email, _Opts) ->
  v_email(V0);
validate(V0, _D, entity_exists, Opts)->
  v_entity_exists(V0, Opts);
validate(V0, _D, entity_not_exists, Opts)->
  v_entity_not_exists(V0, Opts);
validate(_V0, _D, Type, _Opts) ->
  {error, {unknown_validator, Type}}.


-spec v_required(any(), any()) -> {ok, any()} | {error, required}.
v_required(undefined, ?NOT_SET) ->
  {error, required};
v_required(undefined, Default) ->
  {ok, Default};
v_required(V0, _) ->
  {ok, V0}.


-spec v_optional(any(), any()) -> ok | {ok, any()}.
v_optional(undefined, ?NOT_SET) ->
  ok;
v_optional(undefined, Default) ->
  {ok, Default};
v_optional(V0, _) ->
  {ok, V0}.


-spec v_min(any(), integer() | float()) -> {ok, any()} | {error, {min, integer() | float()}}.
v_min(V0, V) when is_integer(V0), is_integer(V) ->
  case V0 >= V of
    true -> {ok, V0};
    _    -> {error, {min, V}}
  end;
v_min(V0, V) when is_float(V0), is_float(V) ->
  case V0 >= V of
    true -> {ok, V0};
    _    -> {error, {min, V}}
  end;
v_min(V0, V) when is_binary(V0), is_integer(V) ->
  case byte_size(V0) >= V of
    true -> {ok, V0};
    _    -> {error, {min, V}}
  end;
v_min(V0, V) when is_list(V0), is_integer(V) ->
  case length(V0) >= V of
    true -> {ok, V0};
    _    -> {error, {min, V}}
  end;
v_min(_V0, V) when is_integer(V) ->
  {error, wrong_value};
v_min(_, V) ->
  ?ERROR("Wrong min validator arguments ~p", [V]),
  {error, wrong_validator}.


-spec v_max(any(), integer() | float()) -> {ok, any()} | {error, {min, integer() | float()}}.
v_max(V0, V) when is_integer(V0), is_integer(V) ->
  case V0 =< V of
    true -> {ok, V0};
    _    -> {error, {max, V}}
  end;
v_max(V0, V) when is_float(V0), is_float(V) ->
  case V0 =< V of
    true -> {ok, V0};
    _    -> {error, {max, V}}
  end;
v_max(V0, V) when is_binary(V0), is_integer(V) ->
  case byte_size(V0) =< V of
    true -> {ok, V0};
    _    -> {error, {max, V}}
  end;
v_max(V0, V) when is_list(V0), is_integer(V) ->
  case length(V0) =< V of
    true -> {ok, V0};
    _    -> {error, {max, V}}
  end;
v_max(_V0, V) when is_integer(V) ->
  {error, wrong_value};
v_max(_, _) ->
  {error, wrong_validator}.


-spec v_minmax(any(), integer() | float()) -> {ok, any()} | {error, {min, integer() | float()}}.
v_minmax(V0, {V1, V2}) when is_integer(V0), is_integer(V1), is_integer(V2) ->
  case (V0 >= V1) and (V0 =< V2)  of
    true -> {ok, V0};
    _    -> {error, {minmax, {V1, V2}}}
  end;
v_minmax(V0, {V1, V2}) when is_float(V0), is_float(V1), is_float(V2) ->
  case (V0 >= V1) and (V0 =< V2) of
    true -> {ok, V0};
    _    -> {error, {minmax, {V1, V2}}}
  end;
v_minmax(V0, {V1, V2}) when is_binary(V0), is_integer(V1), is_integer(V2) ->
  case (byte_size(V0) >= V1) and (byte_size(V0) =< V2) of
    true -> {ok, V0};
    _    -> {error, {minmax, {V1, V2}}}
  end;
v_minmax(V0, {V1, V2}) when is_list(V0), is_integer(V1), is_integer(V2) ->
  case (length(V0) >= V1) and (length(V0) =< V2) of
    true -> {ok, V0};
    _    -> {error, {minmax, {V1, V2}}}
  end;
v_minmax(_V0, {V1, V2}) when is_integer(V1), is_integer(V2) ->
  {error, wrong_value};
v_minmax(_, _) ->
  {error, wrong_validator}.


-spec v_enum(any(), [any()]) -> {ok, float()} | {error, term()}.
v_enum(V0, List) when is_list(List) ->
  case lists:member(V0, List) of
    true  -> {ok, V0};
    false -> {error, {nonof, List}}
  end;
v_enum(_, _) ->
  {error, wrong_validator}.


-spec v_model(any(), atom()) -> {ok, any()} | {error, term()}.
v_model(V0, Model) ->
  Model:validate(V0).


-spec v_regexp(any(), {iodata(), list()}) -> {ok, any()} | {error, term()}.
v_regexp(V0, {RegExp, RegOpts}) when (is_binary(V0) orelse is_list(V0)), is_list(RegOpts) ->
  case re:run(V0, RegExp, RegOpts) of
    {match, _} ->
      {ok, V0};
    _ ->
      {error, wrong_value}
  end;
v_regexp(_,_) ->
  {error, wrong_validator}.


-spec v_key(any()) -> {ok, binary()} | {error, term()}.
v_key(V0) when (is_binary(V0) orelse is_list(V0)) -> 
  case v_clean_string(V0, string) of
    {ok, V2} ->
      ReturnTo =
        case is_list(V0) of
          true -> list;
          _ -> binary
        end,
      case v_regexp(V2, {"^[\\w\\-]*$", [global, caseless, {capture, first, ReturnTo}]}) of
        {ok, V3} ->
          {ok, string:lowercase(V3)};
        Any -> Any
      end;
    _ ->
      {error, wrong_value}
  end;
v_key(_) ->
  {error, wrong_validator}.


-spec v_clean_string(any(), text | string) -> {ok, any()} | {error, term()}.
v_clean_string(String, Type) when (is_binary(String) orelse is_list(String)), (Type == text orelse Type == string) ->
  case v_trim_string(String) of
    {ok, Cleaned} ->
      %% http://erlang.org/doc/man/re.html
      UnicodeSpaces = <<
        16#000B/utf8,

        %% horizontal space characters
        16#00A0/utf8,
        16#1680/utf8,
        16#180e/utf8,
        16#2000/utf8,
        16#2001/utf8,
        16#2002/utf8,
        16#2003/utf8,
        16#2004/utf8,
        16#2005/utf8,
        16#2006/utf8,
        16#2007/utf8,
        16#2008/utf8,
        16#2009/utf8,
        16#200a/utf8,
        16#2028/utf8,
        16#2029/utf8,
        16#202f/utf8,
        16#205f/utf8,
        16#3000/utf8,

        %% vertical space characters
        16#000C/utf8,
        16#000D/utf8,
        16#0085/utf8,
        16#2028/utf8,
        16#2029/utf8>>,
      Pattern = case Type of
                  string -> <<"[\\a\\e\\f\\n\\r\\t", UnicodeSpaces/binary, "]*">>;
                  text -> <<"[\\a\\e\\f\\t", UnicodeSpaces/binary, "]*">>
                end,
      ReturnTo =
        case is_list(String) of
          true -> list;
          _ -> binary
        end,
      {ok, re:replace(Cleaned, Pattern, "", [global, {return, ReturnTo}, unicode])};
    Any -> Any
  end;
v_clean_string(_,_) -> 
  {error, wrong_validator}.


-spec v_trim_string(unicode:chardata()) -> {ok, unicode:chardata()} | {error, term()}.
v_trim_string(V) when (is_binary(V) orelse is_list(V)) ->
  V2 = string:trim(V),
  ReturnTo =
    case is_list(V) of
      true -> list;
      _ -> binary
    end,
  {ok, re:replace(V2, "(^[\\s\\b]*)|([\\s\\b]*$)", "", [global, {return, ReturnTo}])};
v_trim_string(V) ->
  ?ERROR("Wrong trim validator argument ~p", [V]),
  {error, wrong_validator}.


-spec v_escape_string(any(), text | string) -> {ok, any()} | {error, term()}.
v_escape_string(V, Type) when (is_binary(V) orelse is_list(V)), (Type == text orelse Type == string) ->
  case v_clean_string(V, Type) of
    {ok, V2} ->
      case iomod:escape_user_input(V2) of
        {error, _} -> {error, wrong_value};
        V3 -> {ok, V3}
      end;
    Any -> Any
  end;
v_escape_string(_, _) ->
  {error, wrong_validator}.


-spec v_path(any()) -> {ok, binary()} | {error, term()}.
v_path(V0) when (is_binary(V0) orelse is_list(V0)) -> 
  case v_clean_string(V0, string) of
    {ok, V2} ->
      ReturnTo =
        case is_list(V0) of
          true -> list;
          _ -> binary
        end,
      case v_regexp(V2, {"^[\\w\\-/]*$", [global, caseless, {capture, first, ReturnTo}]}) of
        {ok, V3} ->
          {ok, string:lowercase(V3)};
        Any -> Any
      end;
    _ ->
      {error, wrong_value}
  end;
v_path(_) ->
  {error, wrong_validator}.


-spec v_login(any()) -> {ok, binary()} | {error, term()}.
v_login(V0) when (is_binary(V0) orelse is_list(V0)) -> 
  case v_clean_string(V0, string) of
    {ok, V2} ->
      ReturnTo =
        case is_list(V0) of
          true -> list;
          _ -> binary
        end,
      v_regexp(V2, {"^[\\w\\-@\\.]*$", [global, caseless, {capture, first, ReturnTo}]});
    _ ->
      {error, wrong_value}
  end;
v_login(_) ->
  {error, wrong_validator}.


-spec v_json(any()) -> {ok, binary()} | {error, term()}.
v_json(V0) when (is_binary(V0) orelse is_list(V0)) -> 
  try ?FROM_JSON(V0) of
    _ -> {ok, V0}
  catch
    _ -> {error, wrong_value}
  end;
v_json(_) ->
  {error, wrong_validator}.

-spec entity_count(any(), list() | binary() | {integer(), list() | binary()}) -> integer() | {error, term()}.
entity_count(V0, Table) when is_binary(Table) ->
  entity_count(V0, binary_to_list(Table));
entity_count(V0, Table) when is_list(Table)->
  entity_count(V0, {Table, "id = $1"});

entity_count(V0, {Table, Where}) when is_binary(Table), is_list(Where) ->
  entity_count(V0, {binary_to_list(Table), Where});
entity_count(V0, {Table, Where}) when is_list(Table), is_binary(Where) ->
  entity_count(V0, {Table, binary_to_list(Where)});  
entity_count(V0, {Table, Where}) when is_binary(Table), is_binary(Where) ->
  entity_count(V0, {binary_to_list(Table), binary_to_list(Where)});  

entity_count(V0, {Table, Where}) when is_list(Table), is_list(Where) ->
  case db_pg:q(?DBPOOL, "SELECT COUNT(id) as count FROM " ++ Table ++ " WHERE " ++ Where, [V0]) of
    {ok, [#{ count := COUNT}]} ->
      COUNT;
    Error ->
      ?ERROR("Can't find object. ~p", [Error]),
      {error, wrong_validator}
  end;

entity_count(_, _) ->
  {error, wrong_validator}.

-spec v_entity_exists(any(), list() | binary() | {integer(), list() | binary()}) -> ok | {error, term()}.
v_entity_exists(V0, Opt) ->
  case entity_count(V0, Opt) of
    0 -> {error, wrong_value};
    Count when is_integer(Count) -> ok;
    Any -> Any
  end.


-spec v_entity_not_exists(any(), list() | binary() | {integer(), list() | binary()}) -> ok | {error, term()}.
v_entity_not_exists(V0, Opt) ->
  case entity_count(V0, Opt) of
    0 -> ok;
    Count when is_integer(Count) -> {error, wrong_value};
    Any -> Any
  end.


-spec v_email(any()) -> ok | {error, term()}.
v_email(V) when (is_binary(V) orelse is_list(V)) ->
  case v_regexp(V, {"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$", [global, caseless]}) of
    {ok, _} -> {ok, V};
    Any -> {error, wrong_email}
  end;
v_email(_) ->
  {error, wrong_validator}.



