-module(model).
-export([
	map_fields/2,
	coerce/2,
	validate/2,
	default/2,
	do_validate/3
	]).

-include("models.hrl").


-spec map_fields(map() | [{any(), any()}], fun((any()) -> atom())) -> #{atom() => any()} | undefined.
map_fields([], _MapField) ->
	#{};
map_fields([{_, _}|_] = Data, MapField) ->
	maps:from_list(lists:filtermap(
		fun({Key0, Value}) ->
			case MapField(Key0) of
				undefined -> false;
				Key       -> {true, {Key, Value}}
			end
		end, Data));
map_fields(Data, MapField) when is_map(Data) ->
	case maps:size(Data) of
		0 -> #{};
		_ -> map_fields(maps:to_list(Data), MapField)
	end;
map_fields(_Data, _MapField) ->
	undefined.


-spec coerce(any(), atom()) -> coerce_result().
coerce(V, Type) ->
	model_coerce:coerce(V, Type).


-spec validate(#{atom() => any()}, field_defs()) -> validation_result().
validate(Data0, Model) ->
	{Data, Errors} = lists:foldl(
		fun(#field_def{name = Name, validators = Validators}, {D, E}) ->
			case do_validate(maps:get(Name, Data0, undefined), Validators, D) of
				ok              -> {D, E};
				{ok, V}         -> {D#{Name => V}, E};
				{error, Reason} -> {D, E#{Name => Reason}}
			end
		end,
		{#{}, #{}}, Model),
	case maps:size(Errors) of
		0   -> {ok, Data};
		_   -> {error, Errors}
	end.


-spec default(atom(), field_defs()) -> any() | undefined.
default(_Name, []) ->
	undefined;
default(Name, [#field_def{name = Name, opts = #{default := Default}}|_Model]) ->
	Default;
default(Name, [_|Model]) ->
	default(Name, Model).


-spec do_validate(any(), field_validators(), any()) -> ok | {ok, any()} | {error, any()}.
do_validate(undefined, [], _) ->
	ok;
do_validate(V, [], _) ->
	{ok, V};
do_validate(V0, [Validator|Validators], D) ->
	case apply_validator(V0, Validator, D) of
		ok      -> ok;
		{ok, V} -> do_validate(V, Validators, D);
		Any     -> Any
	end.


-spec apply_validator(any(), field_validator(), any()) -> ok | {ok, any()} | {error, any()}.
apply_validator(V0, Validator, D) when is_atom(Validator) ->
	model_validate:validate(V0, D, Validator);	
apply_validator(V0, {Validator, Opts}, D) when is_atom(Validator) ->
	model_validate:validate(V0, D, Validator, Opts);

apply_validator(V0, {Module, Validator, Opts}, D) when is_atom(Module), is_atom(Validator) ->
	Module:Validator(V0, D, Opts);

apply_validator(_V0, Validator, _) ->
	{error, {unknown_validator, Validator} }.