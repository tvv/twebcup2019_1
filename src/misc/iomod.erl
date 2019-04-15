-module(iomod).
-export([
  in/1,
  in/2,
  out/1,
  map_to_json/1,
  list_to_json/1,
  escape_user_input/1,
  new_password/1,
  hash_password/1,
  md5_hex/1,
  random_str/2,
  random_str/3,
  random_num/2
]).

-include("wa.hrl").
-include("models.hrl").


in(Msg) ->
  in(Msg, #{}).
in(Msg, Default) ->
  try ?FROM_JSON(Msg) of
    Data -> Data
  catch Exc:Exp:Stacktrace -> 
    ?ERROR("Exception ~p:~p in decoding of ~p~n~p", [Exc, Exp, Msg, Stacktrace]),
    Default 
  end.


out(Msg) ->
  try ?TO_JSON(Msg) of
    Str -> Str
  catch 
    Exc:Exp:Stacktrace -> 
      ?ERROR("Exception ~p:~p in encoding of ~p~n~p", [Exc, Exp, Msg, Stacktrace]),
      <<"{}">>
  end.


-spec escape_user_input(Data :: list() | binary() | any()  ) -> binary() | {error, wrong_data}.
escape_user_input(Data) when is_list(Data) ->
  escape_user_input(utils:to_binary(Data));
escape_user_input(Data) when is_binary(Data) ->
  inject_spaces_to_long_words(Data);
escape_user_input(_Data) ->
  {error, wrong_data}.

inject_spaces_to_long_words(Data) ->
  utils:concat(lists:map(fun(D) ->
      case string:length(D) > 32 of
        true -> 
          inject_spaces(remove_zero_spaces(D), "");
        false -> 
          escape(D)
      end
    end, string:split(Data, " ", all)), <<" ">>).

remove_zero_spaces(Str) ->
  unicode:characters_to_binary(string:replace(Str, unicode:characters_to_binary([16#200B]), <<>>, all)). 

inject_spaces(Str, NewStr) ->
  case string:length(Str) < 4 of
    true ->
      unicode:characters_to_binary([NewStr, escape(Str)]);
    false ->
      inject_spaces(
          string:slice(Str, 4), 
          unicode:characters_to_binary([NewStr, escape(string:slice(Str, 0, 4)), 16#200B])
        )
  end.

-define(ESC_RULES, [
        {<<"&">>, <<"&amp;">>},
        {<<"<">>, <<"&lt;">>},
        {<<">">>, <<"&gt;">>},
        {<<"\"">>, <<"&quot;">>},
        {<<"'">>, <<"&#x27">>},
        {<<"/">>, <<"&#x2F">>}]).

escape(Data) ->
  escape(Data, ?ESC_RULES).

escape(Data, []) -> Data;
escape(Data, [{Pattern, Replacement} | Chars]) ->
  escape(binary:replace(Data, Pattern, Replacement, [global]), Chars).


-spec new_password(Length) -> {binary(), binary()}
  when
    Length :: integer().
new_password(Length) ->
  Password = random_str(Length, ?PWD_ABC),
  {Password, hash_password(Password)}.


-spec hash_password(binary() | list()) -> binary(). 
hash_password(PlainPwd) when is_list(PlainPwd) ->
  hash_password(list_to_binary(PlainPwd));
hash_password(PlainPwd) when is_binary(PlainPwd) ->
  md5_hex(<<(md5_hex(PlainPwd))/binary, (maps:get(pwd, ?CONFIG(salts, #{}), <<"deadbeef">>))/binary>>).


-spec md5_hex(binary()) -> binary().
md5_hex(Data) when is_binary(Data) ->
  list_to_binary(
    lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(Data)])).


-spec random_str(integer(), list()) -> list().
random_str(Size, ABC) ->
  random_str([], Size, ABC).


-spec random_str(list(), integer(), list()) -> list().
random_str(Prefix, 0, _) ->
  Prefix;
random_str(Prefix, Size, ABC) ->
  N = rand:uniform(length(ABC)),
  C = lists:nth(N, ABC),
  random_str(Prefix ++ [C], Size - 1, ABC).


-spec random_num(integer(), integer()) -> integer().
random_num(Min, Max) ->
  V = rand:uniform(Max - Min),
  V + Min.


-spec map_to_json(map()) -> map().
map_to_json(M) ->
  maps:map(fun
      (_K, V) when is_map(V) -> map_to_json(V);
      (_K, {{_, _, _}, {_, _, _}} = V) -> iso8601:format(V);
      (_K, V) when is_tuple(V) -> list_to_json(tuple_to_list(V));
      (_K, V) when is_list(V) -> list_to_json(V);
      (_K, V) -> V
    end, M).


-spec list_to_json(list()) -> list().
list_to_json(L) ->
  lists:map(fun
      (I) when is_map(I) -> map_to_json(I);
      ({{_, _, _}, {_, _, _}} = I) -> iso8601:format(I);
      (I) when is_tuple(I) -> tuple_to_list(I);
      (I) -> I 
    end, L).