-module(coerce_SUITE).
-include_lib("common_test/include/ct.hrl").
 
-export([
	all/0
]).

-export([
	integer_coerce/1,
	float_coerce/1,
	binary_coerce/1,
	list_of_coerce/1,
	model_coerce/1,
	anyof_coerce/1,
	datetime_coerce/1,
	list_of_model_coerce/1,
	date_coerce/1,
	time_coerce/1,
	duration_coerce/1
	]).
 

all() -> 
	[ integer_coerce,
		float_coerce,
		binary_coerce,
		list_of_coerce,
		model_coerce,
		anyof_coerce,
		datetime_coerce,
		list_of_model_coerce,
		date_coerce,
		time_coerce,
		duration_coerce
	].


integer_coerce(_Config) ->
	{ok, 10} = model_coerce:coerce(10, integer),
	{ok, 10} = model_coerce:coerce(<<"10">>, integer),
	{ok, 10} = model_coerce:coerce("10", integer),
	{error, wrong_value} = model_coerce:coerce(10.0, integer),
	{error, wrong_value} = model_coerce:coerce(<<"not integer">>, integer),
	{error, wrong_value} = model_coerce:coerce("not integer", integer),
	{error, wrong_value} = model_coerce:coerce(any_atom, integer),
	{error, wrong_value} = model_coerce:coerce({tuple}, integer),
	{error, wrong_value} = model_coerce:coerce(#{}, integer).


float_coerce(_Config) ->
	{ok, 10.0} = model_coerce:coerce(10.0, float),
	{ok, 10.0} = model_coerce:coerce(<<"10.0">>, float),
	{ok, 10.0} = model_coerce:coerce("10.0", float),
	{ok, 10.0} = model_coerce:coerce(10, float),
	{ok, 10.0} = model_coerce:coerce(<<"10">>, float),
	{ok, 10.0} = model_coerce:coerce("10", float),
	{error, wrong_value} = model_coerce:coerce(<<"not integer">>, float),
	{error, wrong_value} = model_coerce:coerce("not integer", float),
	{error, wrong_value} = model_coerce:coerce(any_atom, float),
	{error, wrong_value} = model_coerce:coerce({tuple}, float),
	{error, wrong_value} = model_coerce:coerce(#{}, float).


binary_coerce(_Config) ->
	{ok, <<>>} = model_coerce:coerce(<<>>, binary),
	{ok, <<"test">>} = model_coerce:coerce(<<"test">>, binary),
	{ok, <<>>} = model_coerce:coerce("", binary),
	{ok, <<"test">>} = model_coerce:coerce("test", binary),
	{error, wrong_value} = model_coerce:coerce(1, binary),
	{error, wrong_value} = model_coerce:coerce(1.0, binary),
	{error, wrong_value} = model_coerce:coerce(any_atom, binary),
	{error, wrong_value} = model_coerce:coerce({tuple}, binary),
	{error, wrong_value} = model_coerce:coerce(#{}, binary).


list_of_coerce(_Config) ->
	{ok, [1, 2, 3]} = model_coerce:coerce([1, 2, 3], {listof, integer}),
	{ok, [1, 2, 3]} = model_coerce:coerce(["1", <<"2">>, 3], {listof, integer}),
	{ok, [1.0, 2.0, 3.0]} = model_coerce:coerce([1, 2.0, <<"3.0">>], {listof, float}),
	{error, wrong_value} = model_coerce:coerce(1, {listof, integer}),
	{error, wrong_value} = model_coerce:coerce(1.0, {listof, integer}),
	{error, wrong_value} = model_coerce:coerce(any_atom, {listof, integer}),
	{error, wrong_value} = model_coerce:coerce({tuple}, {listof, integer}),
	{error, wrong_value} = model_coerce:coerce(#{}, {listof, integer}).


model_coerce(_Config) ->
	{ok, #{id := 1}} = model_coerce:coerce(#{id => 1}, {model, category}),
	{ok, #{id := 1, name := <<"name">>}} = model_coerce:coerce(#{id => 1, name => <<"name">>}, {model, category}),
	{ok, #{id := 1}} = model_coerce:coerce(#{id => "1"}, {model, category}).


anyof_coerce(_Config) ->
	{ok, 1} = model_coerce:coerce(1, {anyof, [integer, float, boolean]}),
	{ok, 1.0} = model_coerce:coerce(<<"1.0">>, {anyof, [boolean, integer, float]}),
	{ok, 1.0} = model_coerce:coerce("1.0", {anyof, [boolean, integer, float]}),
	{ok, true} = model_coerce:coerce(1, {anyof, [boolean, integer, float]}),

	{error, wrong_value} = model_coerce:coerce(<<"1.0">>, {anyof, [integer, boolean]}),
	{error, wrong_value} = model_coerce:coerce("1.0", {anyof, [integer, boolean]}),
	{error, wrong_value} = model_coerce:coerce(<<"not a number">>, {anyof, [float, integer, boolean]}),
	{error, wrong_value} = model_coerce:coerce("not a number", {anyof, [float, integer, boolean]}).


datetime_coerce(_Config) ->
	{ok, {{2005, 8, 9}, {18, 31, 42.0}}} = model_coerce:coerce("2005-08-09T18:31:42", datetime),
	{ok, {{2005, 8, 9}, {18, 31, 42.0}}} = model_coerce:coerce(<<"2005-08-09T18:31:42">>, datetime),
	{ok, {{2005, 8, 9}, {18, 31, 42.123}}} = model_coerce:coerce(<<"2005-08-09T18:31:42.123">>, datetime),

	{ok, {{2005, 8, 9}, {15, 31, 42.0}}} = model_coerce:coerce(<<"2005-08-09T18:31:42+03">>, datetime),
	{ok, {{2005, 8, 9}, {15, 31, 42.123}}} = model_coerce:coerce(<<"2005-08-09T18:31:42.123+03">>, datetime),

	{error, wrong_value} = model_coerce:coerce(<<"not-a-date">>, datetime),
	{error, wrong_value} = model_coerce:coerce(1, datetime),
	{error, wrong_value} = model_coerce:coerce(<<"wron-gd-atTai:gu:ar.ran+ty">>, datetime).


list_of_model_coerce(_Config) ->
	{ok, [#{id := 1, name := <<"name">>}, #{id := 1, name := <<"name">>}]} = model_coerce:coerce(
		[#{id => 1, name => "name"}, #{id => "1", name => <<"name">>}], 
		{listof, {model, category}}
	),

	{error, #{id := wrong_value}} = model_coerce:coerce(
		[#{id => "1.0", name => atom}], 
		{listof, {model, category}}
	).
	

date_coerce(_Config) ->
	{ok, {2005, 8, 9}} = model_coerce:coerce(<<"2005-08-09">>, date),
	{ok, {2005, 8, 9}} = model_coerce:coerce(<<"20050809">>, date),
	{ok, {2005, 8, 9}} = model_coerce:coerce("2005-08-09", date),

	{error, wrong_value} = model_coerce:coerce(<<"not-a-date">>, date),
	{error, wrong_value} = model_coerce:coerce(1, date).


time_coerce(_Config) ->
	{ok, {18, 31, 42}} = model_coerce:coerce("18:31:42", time),
	{ok, {18, 31, 42}} = model_coerce:coerce(<<"18:31:42">>, time),
	{ok, {18, 31, 42}} = model_coerce:coerce(<<"183142">>, time),
	{ok, {18, 31, 0}} = model_coerce:coerce(<<"18:31">>, time),
	{ok, {18, 31, 0}} = model_coerce:coerce(<<"1831">>, time),

	{error, wrong_value} = model_coerce:coerce("noat", time),
	{error, wrong_value} = model_coerce:coerce(<<"not-a-time">>, time),
	{error, wrong_value} = model_coerce:coerce(1, time).


duration_coerce(_Config) ->
	{ok, [{sign, "+"}, {years, 6}, {months, 3}, {days, 1}, {hours, 1}, {minutes, 1}, {seconds, 1}]} = model_coerce:coerce("+P6Y3M1DT1H1M1.1S", duration),
	{ok, [{sign, "+"}, {years, 6}, {months, 3}, {days, 1}, {hours, 1}, {minutes, 1}, {seconds, 1}]} = model_coerce:coerce(<<"+P6Y3M1DT1H1M1.1S">>, duration),

	{error, wrong_value} = model_coerce:coerce("not-a-duration", duration).