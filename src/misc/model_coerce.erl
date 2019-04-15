-module(model_coerce).
-export([
	coerce/2,
	to_integer/1,
	to_float/1,
	to_binary/1,
	to_boolean/1,
	to_list_of/2,
	to_model/2,
	to_anyof/2,
	to_time/1,
	to_date/1,
	to_datetime/1,
	to_duration/1
	]).

-include("models.hrl").


-spec coerce(any(), field_type()) -> {ok, any()} | {error, term()}.
coerce(V0, integer) ->
	to_integer(V0);
coerce(V0, float) ->
	to_float(V0);
coerce(V0, binary) ->
	to_binary(V0);
coerce(V0, boolean) ->
	to_boolean(V0);
coerce(V0, {listof, Type}) ->
	to_list_of(V0, Type);
coerce(V0, {model, Module}) ->
	to_model(V0, Module);
coerce(V0, {anyof, Types}) ->
	to_anyof(V0, Types);
coerce(V0, time) ->
	to_time(V0);
coerce(V0, date) ->
	to_date(V0);
coerce(V0, datetime) ->
	to_datetime(V0);
coerce(V0, duration) ->
	to_duration(V0);
coerce(_V0, _Type) ->
	{error, wrong_coerce}.


-spec to_integer(any()) -> {ok, integer()} | {error, term()}.
to_integer(V0) when is_integer(V0) ->
	{ok, V0};
to_integer(V0) when is_binary(V0) ->
	try_coerce(V0, fun binary_to_integer/1);
to_integer(V0) when is_list(V0) ->
	try_coerce(V0, fun list_to_integer/1);
to_integer(_) ->
	{error, wrong_value}.


-spec to_float(any()) -> {ok, float()} | {error, term()}.
to_float(V0) when is_integer(V0) ->
	{ok, float(V0)};
to_float(V0) when is_float(V0) ->
	{ok, V0};
to_float(V0) when is_binary(V0) ->
	case try_coerce(V0, fun binary_to_float/1) of
		{ok, _} = V -> V;
		_ ->
			case to_integer(V0) of
				{ok, V} -> to_float(V);
				Any -> Any
			end
	end;
to_float(V0) when is_list(V0) ->
	case try_coerce(V0, fun list_to_float/1) of
		{ok, _} = V -> V;
		_ ->
			case to_integer(V0) of
				{ok, V} -> to_float(V);
				Any -> Any
			end
	end;
to_float(_) ->
	{error, wrong_value}.


-spec to_binary(any()) -> {ok, binary()} | {error, term()}.
to_binary(V0) when is_binary(V0) ->
	{ok, V0};
to_binary(V0) when is_list(V0) ->
	{ok, list_to_binary(V0)};
to_binary(_) ->
	{error, wrong_value}.


-spec to_boolean(any()) -> {ok, boolean()} | {error, term()}.
to_boolean(true)   		  -> {ok, true};
to_boolean("true") 		  -> {ok, true};
to_boolean(<<"true">>)  -> {ok, true};
to_boolean(1) 				  -> {ok, true};	
to_boolean("1") 			  -> {ok, true};
to_boolean(<<"1">>) 	  -> {ok, true};

to_boolean(false) 		  -> {ok, false};
to_boolean("false") 	  -> {ok, false};
to_boolean(<<"false">>) -> {ok, false};
to_boolean(0) 					-> {ok, false};	
to_boolean("0") 			  -> {ok, false};
to_boolean(<<"0">>) 	  -> {ok, false};
to_boolean(_) 					-> {error, wrong_value}.


-spec to_list_of(any(), field_type()) -> {ok, [any()]} | {error, term()}.
to_list_of(V0, Type) when is_list(V0) ->
	coerce_list_of(V0, Type);
to_list_of(_, _) ->
	{error, wrong_value}.


-spec to_model(any(), atom()) -> {ok, any()} | {error, term()}.
to_model(V0, Module) ->
	Module:coerce(V0).


-spec to_anyof(any(), [field_type()]) -> {ok, any()} | {error, term()}.
to_anyof(_, []) ->
	{error, wrong_value};
to_anyof(V0, [Type|Types]) ->
	case coerce(V0, Type) of
		{ok, V} -> {ok, V};
		_ -> to_anyof(V0, Types)
	end.


-spec to_time(any()) -> {ok, time()} | {error, term()}.
to_time(V0) when is_binary(V0); is_list(V0) ->
	Re = [
		{hhmm, "^[ ]*([0-2]\\d):([0-5]\\d)[ ]*$"},
		{hhmm, "^[ ]*([0-2]\\d)([0-5]\\d)[ ]*$"},
		{hhmmss, "^[ ]*([0-2]\\d):([0-5]\\d):([0-5]\\d)[ ]*$"},
		{hhmmss, "^[ ]*([0-2]\\d)([0-5]\\d)([0-5]\\d)[ ]*$"}
	],
	case any_of_re(V0, Re) of
		{ok, {hhmm, [[_, HB, MB]]}} -> 
			case binary_to_integer(HB) of
				H when H >= 0, H =< 24 ->
					{ok, {H, binary_to_integer(MB), 0}};
				_ ->
					{error, wrong_value}
			end;
		{ok, {hhmmss, [[_, HB, MB, SB]]}} -> 
			case binary_to_integer(HB) of
				H when H >= 0, H =< 24 ->
					{ok, {H, binary_to_integer(MB), binary_to_integer(SB)}};
				_ ->
					{error, wrong_value}
			end;
		_ -> 
			{error, wrong_value}
	end;
to_time({H, M, S} = Time) 
		when is_integer(H), is_integer(M), is_integer(S), 
				 H >= 0, H =< 24, M >= 0, M =< 60, S >= 0, S =< 60 ->
	{ok, Time};
to_time({H, M}) 
		when is_integer(H), is_integer(M), H >= 0, H =< 24, M >= 0, M =< 60 ->
	{ok, {H, M, 0}};
to_time(_V0) ->
	{error, wrong_value}.


-spec to_date(any()) -> {ok, date()} | {error, term()}.
to_date(V0) when is_binary(V0) orelse is_list(V0) ->
	case try_coerce(V0, fun iso8601:parse_exact/1) of
		{ok, {Date, _}} -> {ok, Date};
		_         -> {error, wrong_value}
	end;
to_date({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
	{ok, Date};
to_date({{Y, M, D}, _} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
	{ok, Date};
to_date(_V0) ->
	{error, wrong_value}.


-spec to_datetime(any()) -> {ok, datetime()} | {error, term()}.
to_datetime(V0) when is_binary(V0) orelse is_list(V0) ->
	try_coerce(V0, fun iso8601:parse_exact/1);
to_datetime({MaybeDate, MaybeTime}) ->
	case {to_date(MaybeDate), to_time(MaybeTime)} of
		{{ok, Date}, {ok, Time}} -> {ok, {Date, Time}};
		_ 											 -> {error, wrong_value}
	end;
to_datetime(_V0) ->
	{error, wrong_value}.


-spec to_duration(any()) -> {ok, duration()} | {error, term()}.
to_duration(V0) when is_binary(V0) orelse is_list(V0) ->
	try_coerce(V0, fun iso8601:parse_duration/1);
to_duration(_V0) ->
	{error, wrong_value}.


coerce_list_of(V0, Type) ->
	coerce_list_of(V0, Type, []).

coerce_list_of([], _, Acc) ->
	{ok, Acc};
coerce_list_of([I0|V0], Type, Acc) ->
	case coerce(I0, Type) of
		{ok, I} -> coerce_list_of(V0, Type, Acc ++ [I]);
		Any     -> Any
	end.


try_coerce(V0, Fun) ->
	try Fun(V0) of
		V   -> {ok, V}
	catch
		_:_ ->
			{error, wrong_value}
	end.


any_of_re(_V0, []) ->
	nomatch;
any_of_re(V0, [{Type, Re}|Rest]) ->
	case re:run(V0, Re, [global, {capture, all, binary}]) of
		{match, Captured} -> {ok, {Type, Captured}};
		_                 -> any_of_re(V0, Rest)
	end.
