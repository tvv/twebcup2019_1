-module(model_pt).
-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("models.hrl").


%%%%
% Model parse transform implement range of coerse and validation functions.
% 
% To use Model parse transform model definition must be defined. D
% Definition looks like%
% -model([
%     field name    coerce            validation rules 
%     {id         , integer,          [{min, 1}], #{ default => 0, pk => true }},
%     ...
%  ]).
%
% Function added to implementation:
% - definition/0
% - new/0
% - default/0
% - default/1
% - field_count/0
% - pk/0
% - map_field/1
% - map_fileds/1
% - validate/1
% - coerce/1
%
%%%%


parse_transform(Forms0, _Options) ->
	% io:format("Forms0 - ~p~n", [Forms0]),

	Funcs = collect_funcs(Forms0),
	NewForms = do_transform(Funcs, Forms0),

	% File = parse_trans:get_file(Forms0),
	% SrcPath = File ++ ".result",
	% parse_trans:pp_src(NewForms, SrcPath),
	erl_syntax:revert_forms(NewForms).


do_transform(Funcs, Forms0) ->
	do_transform(Funcs, Forms0, []).

do_transform(_Funcs, [], Acc) ->
	Acc;
do_transform(Funcs, [{attribute, _, model, Model0} | Forms0], Acc) when is_list(Model0) ->
	do_transform(Funcs, Forms0, Acc ++ codegen_model(Funcs, #{ default => prepare_model(Model0) }));
do_transform(Funcs, [{attribute, _, model, Model0} | Forms0], Acc) when is_map(Model0) ->
	Model = maps:map(fun(_K, V) -> prepare_model(V) end, Model0),
	do_transform(Funcs, Forms0, Acc ++ codegen_model(Funcs, Model));
do_transform(Funcs, [Form|Forms0], Acc) ->
	do_transform(Funcs, Forms0, Acc ++ [Form]).

prepare_model(Model) -> 
	lists:map(fun({Name, Type, Validators, Opts}) ->  
			#field_def{name = Name, type = Type, validators = Validators, opts = Opts}
		end, Model).

codegen_model(DefinedFuncs, Model0) ->
	AbstractModel = erl_syntax:abstract(Model0),
	{Exports, Funcs} = lists:foldl(
		fun({Exports, Funcs}, {AccExports, AccFuncs}) ->  {AccExports ++ Exports, AccFuncs ++ Funcs} end,
		{[], []}, [
			definition(DefinedFuncs, AbstractModel),
			default(DefinedFuncs, Model0),
			new(DefinedFuncs, Model0),
			field_count(DefinedFuncs, Model0),
			pk(DefinedFuncs, Model0),
			map_field(DefinedFuncs, Model0),
			map_fields(DefinedFuncs, Model0),
			coerce(DefinedFuncs, Model0),
			validate(DefinedFuncs, Model0)
		]),
	Exports ++ Funcs.


definition(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {definition, 1}, fun(Model) ->
		{ [
				erl_syntax:revert(?Q("-export([models/0]).")),
				erl_syntax:revert(?Q("-export([definition/0]).")),
				erl_syntax:revert(?Q("-export([definition/1])."))
			], 
			[
				erl_syntax:revert(?Q("models() -> _@Model.")),	
				erl_syntax:revert(?Q("definition() -> definition(default).")),
				erl_syntax:revert(?Q("definition(ModelId) -> maps:get(ModelId, models())."))
			] }
	end, Model0).


new(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {new, 1}, fun(Model) ->
		Fields = maps:fold(fun(Key, Model1, AccM) -> 
				Fs = lists:foldl(fun
						(#field_def{name = Name, opts = #{ pk := true}}, Acc) -> 
							Acc;

						(#field_def{name = Name, opts = #{ default := _}}, Acc) -> 
							NameL = atom_to_list(Name),
							Acc ++ [NameL ++ " => default(" ++ atom_to_list(Key) ++ ", " ++ NameL ++ ")"];
							
						(_, Acc) -> 
							Acc
					end, [], Model1),
				AccM ++ [atom_to_list(Key) ++ " => #{ " ++ lists:concat(lists:join(",", Fs)) ++ " }"]
			end, [], Model),
		{ [
				erl_syntax:revert(?Q("-export([new/0]).")),
				erl_syntax:revert(?Q("-export([new/1])."))
			], 
			[
				erl_syntax:revert(?Q(["new() -> new(default)."])),
				erl_syntax:revert(?Q(["new(ModelId) -> maps:get(ModelId, #{ "] ++ lists:join(",", Fields) ++ ["})."]))

			] }
	end, Model0).


default(DefinedFuncs, Model0) ->	
	if_func_not_defined(DefinedFuncs, {default, 2}, fun(_Model) ->
		{ [
				erl_syntax:revert(?Q("-export([default/1]).")),
				erl_syntax:revert(?Q("-export([default/2])."))
			], 
			[
				erl_syntax:revert(?Q("default(Name) -> model:default(Name, definition(default)).")),
				erl_syntax:revert(?Q("default(ModelId, Name) -> model:default(Name, definition(ModelId))."))
			] }
	end, Model0).


field_count(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {field_count, 1}, fun(_Model) ->
		{ [
				erl_syntax:revert(?Q("-export([field_count/0]).")),
				erl_syntax:revert(?Q("-export([field_count/1])."))
			], 
			[
				erl_syntax:revert(?Q("field_count() -> field_count(default).")),
				erl_syntax:revert(?Q("field_count(ModelId) -> length(definition(ModelId))."))
			] }
	end, Model0).


pk(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {pk, 0}, fun(Model) ->
		Pk = lists:filter(fun
				(#field_def{opts = #{ pk := true }}) -> true;
				(_) -> false
			end, maps:get(default, Model)),
		AbstractModel = erl_syntax:abstract(Pk),
		{ [
				erl_syntax:revert(?Q("-export([pk/0])."))
			], 
			[ 
				erl_syntax:revert(?Q("pk() -> _@AbstractModel."))
			] }
	end, Model0).


map_field(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {map_field, 2}, fun(Model) ->
		{Funcs, _} = maps:fold(fun(_Key, Mod, {AccM, AlreadyInM}) -> 
			lists:foldl(fun(#field_def{name = Name}, {Acc, AlreadyIn}) -> 
					case lists:any(fun(I) -> I =:= Name end, AlreadyIn) of
						true ->
							{Acc, AlreadyIn};
						false ->
							NameL = atom_to_list(Name),
							{
								Acc ++ [
									"map_field(" ++ NameL ++ ") -> " ++ NameL ++ ";",
									"map_field(\"" ++ NameL ++ "\") -> " ++ NameL ++ ";",
									"map_field(<<\"" ++ NameL ++ "\">>) -> " ++ NameL ++ ";"
								],
								AlreadyIn ++ [Name]
							}
					end
				end, {AccM, AlreadyInM}, Mod)
			end, {[], []}, Model),
		{ [
				erl_syntax:revert(?Q("-export([map_field/1])."))
			], 
			[ 
				erl_syntax:revert(?Q(Funcs ++ ["map_field(_) -> undefined."]))
			] }
	end, Model0).


map_fields(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {map_fields, 2}, fun(_Model) ->
		{ [
				erl_syntax:revert(?Q("-export([map_fields/1])."))
			], 
			[ 
				erl_syntax:revert(?Q(["map_fields(Data0) -> model:map_fields(Data0, fun map_field/1)."]))
			] }
	end, Model0).


coerce(DefinedFuncs, Model0) ->
	{Exports1, Funcs1} = if_func_not_defined(DefinedFuncs, {coerce, 2}, fun(_Model) ->
		{ [
				erl_syntax:revert(?Q("-export([coerce/1]).")),
				erl_syntax:revert(?Q("-export([coerce/2])."))
			], 
			[
				erl_syntax:revert(?Q(["coerce(Data0) -> coerce(default, Data0)."])),
				erl_syntax:revert(?Q([
					"coerce(ModelId, Data0) -> ",
						"case map_fields(Data0) of",
							"undefined -> ",
								"{error, wrong_data};",
							"MappedData -> ",
								"{Data, Errors} = maps:fold(",
										"fun(K, V0, {D, E}) ->",
											"case coerce(ModelId, K, V0) of"
												"{ok, V}         -> {D#{K => V}, E};",
												"{error, Reason} -> {D, E#{K => Reason}}",
											"end",
										"end,",
										"{#{}, #{}}, MappedData),",
								"case maps:size(Errors) of",
									"0   -> {ok, Data};",
									"_   -> {error, Errors}",
								"end",
						"end."
					]))
			] }
	end, Model0),
	{Exports2, Funcs2} = if_func_not_defined(DefinedFuncs, {coerce, 3}, fun(Model) ->
		{Defs, Vars} = maps:fold(fun(Key, Mod, {DM, VM}) ->
				ModelIdL = atom_to_list(Key),
				lists:foldl(fun
						(#field_def{name = Name, type = Type}, {D, V}) -> 
							NameL = atom_to_list(Name),
							Ptr =  ModelIdL ++ "_" ++ NameL,
							{ 
								D ++ ["coerce(" ++ ModelIdL ++ ", " ++ NameL ++ ", Value) -> model:coerce(Value, _@" ++ Ptr ++ ");"], 
								V ++ [{list_to_atom(Ptr), erl_syntax:abstract(Type)}]
							}
					end, {DM, VM}, Mod)				
			end, {[], []}, Model),
		{ [
				erl_syntax:revert(?Q("-export([coerce/3])."))
			], 
			[
				erl_syntax:revert(?Q(
					Defs ++ ["coerce(_, Name, _) -> {error, {unknown_field, Name}}."],
					Vars))
			] }
	end, Model0),
	{Exports1 ++ Exports2, Funcs1 ++ Funcs2}.


validate(DefinedFuncs, Model0) ->
	if_func_not_defined(DefinedFuncs, {validate, 2}, fun(_Model) ->
		{ [
				erl_syntax:revert(?Q("-export([validate/1]).")),
				erl_syntax:revert(?Q("-export([validate/2])."))
			], 
			[ 
				erl_syntax:revert(?Q("validate(Data0) -> validate(default, Data0).")),
				case has_post_validate(DefinedFuncs) of
					true -> 
						erl_syntax:revert(?Q([
							"validate(ModelId, Data0) ->",
								"case coerce(ModelId, Data0) of",
									"{ok, Data} -> ",
										"case model:validate(Data, definition(ModelId)) of",
											"{ok, ValidData} -> post_validate(ModelId, ValidData);",
											"Any             -> Any",
										"end;"
									"Any -> Any",
								"end."]));
					false ->
						erl_syntax:revert(?Q([
							"validate(ModelId, Data0) ->",
								"case coerce(ModelId, Data0) of",
									"{ok, Data} -> model:validate(Data, definition(ModelId));",
									"Any 				-> Any",
								"end."]))
				end
			] }
	end, Model0).


collect_funcs(Forms0) ->
	collect_funcs(Forms0, []).

collect_funcs([], Acc) ->
	Acc;
collect_funcs([{function, _, Func, Arity, _}|Forms0], Acc) ->
	collect_funcs(Forms0, [{Func, Arity}|Acc]);
collect_funcs([_|Forms0], Acc) ->
	collect_funcs(Forms0, Acc).


if_func_not_defined(DefinedFuncs, Current, Fun, Model) ->
	case lists:any(fun(I) -> I == Current end, DefinedFuncs) of
		false ->
			Fun(Model);
		_ ->
			{ [], [] }
	end.


has_post_validate(DefinedFuncs) ->
	lists:any(
		fun
			({post_validate, _}) -> true;
			(_) 								 -> false
		end, DefinedFuncs).
