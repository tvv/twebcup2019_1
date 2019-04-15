-module(test_model).
-compile({parse_transform, model_pt}).
-export([
	post_validate/2,
	custom_validator/3
	]).

-include("models.hrl").


-model([
		{id,   integer, [{min, 1}], #{ default => 0, pk => true }},
		{size, float,  [optional], #{}},
		{name, binary,  [optional], #{ default => <<>> }},
		{num1, integer,  [optional, {test_model, custom_validator, 1}], #{}}
	]).


post_validate(_ModelId, #{id := Id}) when Id > 10 ->
	{error, #{id => post_validate_error}};
post_validate(_ModelId, Data) ->
	{ok, Data}.


custom_validator(Value, _Data, Options) ->
	case Value >= Options of
		true -> {ok, Value};
		_    -> {error, wrong_value}
	end.
	
	