-module(validate_SUITE).
-include_lib("common_test/include/ct.hrl").
 
-export([
	all/0
]).

-export([
	required_validate/1,
	optional_validate/1,
	min_validate/1,
	max_validate/1,
	minmax_validate/1,
	model_validate/1,
	model_post_validate/1,
	model_custom_validate/1,
	enum_validate/1,
	key_validate/1,
	clean_string_validate/1,
	trim_string_validate/1,
	escape_string_validate/1,
	path_validate/1,
  regexp_validate/1,
  login_validate/1,
  json_validate/1,
  entity_exists_validate/1
	]).
 

all() -> 
	[ required_validate,
		optional_validate,
		min_validate,
		max_validate,
		minmax_validate,
		model_validate,
		model_post_validate,
		model_custom_validate,
		enum_validate,
		trim_string_validate,
    clean_string_validate,
    key_validate,
		escape_string_validate,
		path_validate,
    regexp_validate,
    login_validate,
    json_validate
    % entity_exists_validate
	].


required_validate(_Config) ->
	{ok, 1} = model_validate:validate(1, undefined, required),
	{error, required} = model_validate:validate(undefined, undefined, required).


optional_validate(_Config) ->
	{ok, 1} = model_validate:validate(1, undefined, optional),
	{ok, 1} = model_validate:validate(undefined, undefined, optional, 1),
	ok = model_validate:validate(undefined, undefined, optional).


min_validate(_Config) ->
	{ok, 1} = model_validate:validate(1, undefined, min, 0),
	{ok, 1} = model_validate:validate(1, undefined, min, 1),
	{error, {min, 2}} = model_validate:validate(1, undefined, min, 2),
	{ok, <<"abc">>} = model_validate:validate(<<"abc">>, undefined, min, 2),
	{ok, "abc"} = model_validate:validate("abc", undefined, min, 2),
	{error, {min, 2}} = model_validate:validate(<<"n">>, undefined, min, 2),
	{error, {min, 2}} = model_validate:validate("n", undefined, min, 2),
	{error, wrong_validator} = model_validate:validate("abc", undefined, min, <<"wrong limit">>).


max_validate(_Config) ->
	{ok, 10} = model_validate:validate(10, undefined, max, 10),
	{ok, 10} = model_validate:validate(10, undefined, max, 11),
	{error, {max, 9}} = model_validate:validate(10, undefined, max, 9),
	{ok, <<"abc">>} = model_validate:validate(<<"abc">>, undefined, max, 3),
	{ok, "abc"} = model_validate:validate("abc", undefined, max, 3),
	{error, {max, 2}} = model_validate:validate(<<"abc">>, undefined, max, 2),
	{error, {max, 2}} = model_validate:validate("abc", undefined, max, 2),
	{error, wrong_validator} = model_validate:validate("abc", undefined, max, <<"wrong limit">>).


minmax_validate(_Config) ->
	{ok, 10} = model_validate:validate(10, undefined, minmax, {10, 11}),
	{ok, 10} = model_validate:validate(10, undefined, minmax, {9, 10}),
	{error, {minmax, {8, 9}}} = model_validate:validate(10, undefined, minmax, {8, 9}),
	{error, {minmax, {8, 9}}} = model_validate:validate(7, undefined, minmax, {8, 9}),
	{ok, <<"abc">>} = model_validate:validate(<<"abc">>, undefined, minmax, {2,3}),
	{ok, "abc"} = model_validate:validate("abc", undefined, minmax, {2,3}),
	{error, {minmax, {1,2}}} = model_validate:validate(<<"abc">>, undefined, minmax, {1,2}),
	{error, {minmax, {1,2}}} = model_validate:validate("abc", undefined, minmax, {1,2}),
	{error, wrong_validator} = model_validate:validate("abc", undefined, minmax, <<"wrong limit">>).


model_validate(_Config) ->
	{ok, #{ id := 1}} = model_validate:validate(
		#{id => 1}, undefined, model, test_model),
	{error, #{ id := wrong_value }} = model_validate:validate(
		#{id => <<"not integer">>}, undefined, model, test_model).


model_post_validate(_Config) ->
	{error, #{ id := post_validate_error }} = model_validate:validate(
		#{id => 11}, undefined, model, test_model).


enum_validate(_Config) ->
	{ok, 1} = model_validate:validate(1, undefine, enum, [1,2,3,4,5]),
	{ok, 5} = model_validate:validate(5, undefine, enum, [1,2,3,4,5]),
	{error, {nonof, [1,2,3,4,5]}} = model_validate:validate(6, undefined, enum, [1,2,3,4,5]),
	{error, {nonof, [1,2,3,4,5]}} = model_validate:validate(0, undefined, enum, [1,2,3,4,5]).


model_custom_validate(_Config) ->
	{ok, #{ id := 1, num1 := 2}} = model_validate:validate(
		#{id => 1, num1 => 2}, undefined, model, test_model),

	{error, #{ num1 := wrong_value }} = model_validate:validate(
		#{id => 1, num1 => 0}, undefined, model, test_model).


trim_string_validate(_Config) ->
  {ok, "string"} = model_validate:validate("string", undefined, trim),
  {ok, <<"string">>} = model_validate:validate(<<"   string  ">>, undefined, trim),
  {ok, <<"string with  space">>} = model_validate:validate(<<"string with  space  ">>, undefined, trim).


clean_string_validate(_Config) ->
  {ok, "string"} = model_validate:validate("string", undefined, clean, string),
  {ok, <<"stringa">>} = model_validate:validate(<<"string\e\f\n\t\ra">>, undefined, clean, string),
  {ok, <<"string\na">>} = model_validate:validate(<<"string\e\f\t\n\ra">>, undefined, clean, text).


key_validate(_Config) ->
  {ok, "string123"} = model_validate:validate("sTrInG123", undefined, key),
  {ok, <<"string123">>} = model_validate:validate(<<"  sTrInG123  ">>, undefined, key),
  {error, wrong_value} = model_validate:validate("sTrInG123!&%", undefined, key).


escape_string_validate(_Config) ->
  {ok, D1} = model_validate:validate("12345678901234567890123456789012\n3", undefined, escape, string),
  <<"1234 5678 9012 3456 7890 1234 5678 9012 3">> = binary:replace(D1, <<226, 128, 139>>, <<" ">>, [global]),
  {ok, D2} = model_validate:validate("12345678901234567890123456789012\n3", undefined, escape, text),
  <<"1234 5678 9012 3456 7890 1234 5678 9012 \n3">> = binary:replace(D2, <<226, 128, 139>>, <<" ">>, [global]),

  {ok, <<"12345678901234567890123456789012">>} = model_validate:validate("12345678901234567890123456789012", undefined, escape, string),
  {ok, <<"12345678901234567890123456789012">>} = model_validate:validate("12345678901234567890123456789012", undefined, escape, text),

  {error, {unknown_validator, escape}} = model_validate:validate("1", undefined, escape, undef).


path_validate(_Config) ->
  {ok, "path/to/something"} = model_validate:validate("pAtH/To/SoMeThInG", undefined, path),
  {error, wrong_value} = model_validate:validate("pAtH/To/SoMeThInG\nnot!valid", undefined, path).


login_validate(_Config) ->
  {ok, "itsmyLogin123"} = model_validate:validate("itsmyLogin123", undefined, login),
  {ok, "new@login."} = model_validate:validate("new@login.", undefined, login),

  {error, wrong_value} = model_validate:validate("is it log\nin?", undefined, login),
  {error, wrong_value} = model_validate:validate("i'm not sure", undefined, login).
  

regexp_validate(_Config) ->
  {ok, "any.match@mail.net"} = model_validate:validate("any.match@mail.net", undefined, regexp, {"^[^@<> ]+@[^@<> ]+\\.[^@<> ]{2,}$", [global, caseless, {capture, first, binary}]}),
  {error, wrong_value} = model_validate:validate("not a match", undefined, regexp, {"^[^@<> ]+@[^@<> ]+\\.[^@<> ]{2,}$", [global, caseless, {capture, first, binary}]}).


json_validate(_Config) ->
  {ok, "{}"} = model_validate:validate("{}", undefined, json),
  {ok, <<"{}">>} = model_validate:validate(<<"{}">>, undefined, json),
  {ok, "{\"value\":\"str\"}"} = model_validate:validate("{\"value\":\"str\"}", undefined, json),

  {error, wrong_value} = model_validate:validate("not_a_json", undefined, json),
  {error, wrong_value} = model_validate:validate("{\"broken\"}:\"json\"", undefined, json).


entity_exists_validate(_Config) ->
  ok = model_validate:validate(10200, undefined, entity_exists, "users"),
  ok = model_validate:validate(10200, undefined, entity_exists, <<"users">>),
  ok = model_validate:validate(10200, undefined, entity_exists, {"users", <<"">>}),
  ok = model_validate:validate(10200, undefined, entity_exists, {<<"users">>, ""}),
  ok = model_validate:validate(10200, undefined, entity_exists, {<<"users">>, <<"meta='{}'">>}),

  {error, wrong_value} = model_validate:validate(10201, undefined, entity_exists, "users"),
  {error, wrong_validator} = model_validate:validate(10201, undefined, entity_exists, "not_a_table"),
  {error, wrong_value} = model_validate:validate(10201, undefined, entity_exists, {"users", "name='not_a_username'"}).