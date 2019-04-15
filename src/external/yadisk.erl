-module(yadisk).
-export([
	files/2,
	resources/2,
	resource_embedded/1,
	resource_items/1,
	upload/3,
	preview/2,
	download/2
]).

-include("wa.hrl").

-type resource() :: #{binary() => any()}.

-type resource_list() :: #{binary() => any()}.

-type upload_resource() :: #{binary() => any()}.

-type resource_params() :: #{
	path   => binary(),
	fields => binary() | undefined,
	limit  => integer() | undefined,
	offset => integer() | undefined,
	preview_crop => binary() | undefined,
	preview_size => binary() | undefined,
	sort   => binary() | undefined
}.


-export_type([
	resource/0,
	resource_list/0,
	resource_params/0
]).


-spec files(Token, Params) -> {ok, ResourceOut} | {error, term()}
	when
		Token       :: binary(),	
		Params      :: resource_params(),
		ResourceOut :: map().
files(Token, Params) ->
	Uri = <<"https://cloud-api.yandex.net/v1/disk/resources/files">>,
	Headers = yaoauth:auth_headers(Token),
	Body = build_resources_params(Params),
	case rest:request(get, qs, Uri, [], Headers, Body) of
		{ok, 200, _, Response} ->			
			{ok, Response};

		{_, _, [], Response} ->
			?ERROR("Failed to get yandex disc info ~p", [Response]),
			{error, request_error}
	end.


-spec resources(Token, Params) -> {ok, Resource} | {error, term()}
	when
		Token    :: binary(),
		Params   :: resource_params(),
		Resource :: resource().
resources(Token, Params) ->
	Uri = <<"https://cloud-api.yandex.net/v1/disk/resources">>,
	Headers = yaoauth:auth_headers(Token),
	Body = build_resources_params(Params),
	case rest:request(get, qs, Uri, [], Headers, Body) of
		{ok, 200, _, Response} ->			
			{ok, Response};

		{_, _, [], Response} ->
			?ERROR("Failed to get yandex disc info ~p", [Response]),
			{error, request_error}
	end.


-spec upload(Token, File, Path) -> {ok, Resource} | {error, term()}
	when
		Token    :: binary(),
		File     :: binary(),
		Path     :: binary(),
		Resource :: resource().
upload(Token, File, Path) ->
	case get_upload_uri(Token, Path) of
		{ok, #{ <<"href">> := Uri, <<"method">> := MethodName}} ->
			Method =
				case MethodName of
					<<"PUT">> -> put;
					_         -> post
				end,
			case rest:request(Method, binary, Uri, [], #{}, File) of
				{ok, Status, _, _} when Status =:= 201; Status =:= 409 ->			
					resources(Token, #{
						path   => Path,
						limit  => 1,
						offset => 0
						});

				{_, _, [], Response} ->
					?ERROR("Failed to get yandex disc info ~p", [Response]),
					{error, request_error}
			end;

		Any ->
			Any
	end.


-spec get_upload_uri(Token, Path) -> {ok, Resource | uploaded} | {error, term()}
	when
		Token    :: binary(),
		Path     :: binary(),
		Resource :: upload_resource().
get_upload_uri(Token, Path) ->
	Uri = "https://cloud-api.yandex.net/v1/disk/resources/upload",
	Headers = yaoauth:auth_headers(Token),
	Body = #{
		<<"path">> => Path
	},
	case rest:request(get, qs, Uri, [], Headers, Body) of
		{ok, 200, _, Response} ->			
			{ok, Response};

		{ok, 409, _, _} ->
			{ok, uploaded};

		{_, _, [], Response} ->
			?ERROR("Failed to get yandex upload uri ~p", [Response]),
			{error, request_error}
	end.



-spec resource_embedded(Resource) -> {ok, ResourceList} | {error, term()}
	when
		Resource     :: resource(),
		ResourceList :: resource_list().
resource_embedded(#{<<"_embedded">> := Embedded}) ->
	{ok, Embedded};
resource_embedded(_) ->
	{error, not_found}.


-spec resource_items(Resource) -> [Resource]
	when
		Resource :: resource().
resource_items(#{<<"_embedded">> := #{ <<"items">> := Items }}) ->
	Items;
resource_items(_) ->
	[].


-spec build_resources_params(Params) -> {ok, RequestParams}
	when
		Params        :: resource_params(),
		RequestParams :: #{binary() => any()}.
build_resources_params(Params) ->
	maps:fold(
		fun
			(_, undefined, Acc) -> Acc;
			(limit, V, Acc)     -> maps:put(<<"limit">>, integer_to_binary(V), Acc);
			(offset, V, Acc)    -> maps:put(<<"offset">>, integer_to_binary(V), Acc);
			(K, V, Acc)         -> maps:put(atom_to_binary(K, utf8), V, Acc)
		end, 
		#{}, Params).


-spec preview(Token, Uri) -> {ok, File} | {error, term()}
	when
		Token :: binary(), 
		Uri   :: binary(),
		File  :: binary().
preview(Token, Uri) ->
	Headers = yaoauth:auth_headers(Token),
	case rest:request(get, binary, Uri, [], Headers, #{}) of
		{ok, 200, _, Data} ->			
			{ok, Data};

		{ok, 302, ResponseHeaders, _} ->	
			case proplists:get_value(<<"location">>, ResponseHeaders) of
				undefined ->
					?ERROR("Failed to get yandex upload uri ~p", [ResponseHeaders]),
					{error, request_error};
				
				NewUri ->
					preview(Token, NewUri)
			end;

		{_, _, [], Response} ->
			?ERROR("Failed to get yandex upload uri ~p", [Response]),
			{error, request_error}
	end.


-spec download(Token, Path) -> {ok, File} | {error, term()}
	when
		Token :: binary(), 
		Path  :: binary(),
		File  :: binary().
download(Token, Path) ->
	Uri = <<"https://cloud-api.yandex.net/v1/disk/resources/download">>,
	Headers = yaoauth:auth_headers(Token),
	Body = #{
		<<"path">> => Path
	}, 
	case rest:request(get, qs, Uri, [], Headers, Body) of
		{ok, 200, _, #{ <<"href">> := ImageUri }} ->			
			download_uri(Token, ImageUri);

		{_, _, [], Response} ->
			?ERROR("Failed to get yandex upload uri ~p", [Response]),
			{error, request_error}
	end.


download_uri(Token, Uri) ->
	Headers = yaoauth:auth_headers(Token),
	case rest:request(get, binary, Uri, [], Headers, #{}) of
		{ok, 200, _, Data} ->
			{ok, Data};

		{ok, 302, ResponseHeaders, _} ->
			?INFO("~p", [ResponseHeaders]),
			case proplists:get_value(<<"location">>, ResponseHeaders) of
				undefined ->
					?ERROR("Failed to get origin image"),
					{error, request_error};

				NewUri ->
					download_uri(Token, NewUri)
			end;

		Any ->
			?ERROR("Failed to get origin image ~p", [Any]),
			{error, request_error}
	end.
