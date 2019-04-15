-type model_id() :: atom().
-type field_defs() :: #{ model_id() => [field_def()] }.

-record(field_def, {
	name       			 :: atom(),
	type       			 :: field_type(), 
	validators = []  :: field_validators(), 
	opts 			 = #{} :: field_opts()
	}).

-type field_def() :: #field_def{}.

-type field_type()  :: 
	binary |
	boolean |
	integer | 
	float | 
	string | 
	date | 
	datetime | 
	{model, field_type()} | 
	field_array_types() |
	{anyof, [field_type()]}.

-type field_array_types() :: integer_array.

-type field_validators() :: [field_validator()].
-type field_validator()  :: 
	atom() | 
	{atom(), any()} |
	{atom(), atom(), any()}.

-type field_opts() :: #{
	default => any(),
	pk 		  => boolean()
	}.

-type validation_result() :: 
	{ok, #{atom() => any()}} | {error, validation_errors() | wrong_data | wrong_validator}.

-type validation_errors() :: 
	#{atom() => {atom(), validation_error_args()} | atom()}.

-type validation_error_args() :: map().

-type coerce_result() :: 
	{ok, any()} | {error, {unknown_field, atom()} | {unknown_coerce, atom()} | any()}.


-type date() 				:: calendar:date().
-type datetime() 	  :: {date(), time()}.
-type hour() 			  :: calendar:hour().
-type microsecond() :: 0..1000000.
-type minute() 			:: calendar:minute().
-type now() 	      :: {integer(), integer(), integer()}.
-type second()      :: calendar:second().
-type time()   	    :: {hour(), minute(), second() | float()}  | {hour(), minute(), second(), microsecond()}.
-type duration()    :: [{atom(), integer()}].