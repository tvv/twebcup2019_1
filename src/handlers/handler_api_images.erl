-module (handler_api_images).
-export([
	init/2
	]).

-include("wa.hrl").


init(Req0, State0) ->
	PathInfo = cowboy_req:path_info(Req0),
  try process(PathInfo, Req0, State0) of
    {ok, Req, State} -> 
      {ok, Req, State}
  catch 
    Ex:Exv:Stacktrace ->
      ?ERROR("Got exception ~p:~p\n~p", [Ex, Exv, Stacktrace]),
      Req = cowboy_req:reply(
      	500,
		    #{<<"content-type">> => <<"application/json">>},
		    <<"{}">>,
		    Req0),
      {ok, Req, State0}
  end.


-spec process(PathInfo, Req, State) -> {ok, Req, State}
	when
		PathInfo :: [binary()],
		Req      :: cowboy_req:req(),
		State    :: map().
process([], #{method := <<"POST">>} = Req0, State0) ->
	http_handler:auth_required(fun(#{ token := Token }) ->
		case http_body:body(Req0) of
			{ok, #{ <<"file">> := #{ filename := FileName, data := File, content_type := CT } }, Req} ->
				case lists:member(CT, valid_mimetypes())  of
					true ->
						case yadisk:upload(Token, File, <<"app:/", FileName/binary>>) of
							{ok, uploaded} ->
								Req1 = http_handler:success_json_response(#{}, Req0),
								{ok, Req1, State0};

							{ok, Resource} ->
								Req1 = http_handler:success_json_response(iomod:map_to_json(Resource), Req),
								{ok, Req1, State0};

							_ ->
								Req1 = http_handler:errormessage_json_response(<<"Не удалось загрузить изображение"/utf8>>, Req),
								{ok, Req1, State0}
						end;

					false ->
						Req1 = http_handler:errormessage_json_response(<<CT/binary, " не поддерживается"/utf8>>, Req0),
						{ok, Req1, State0}
				end;

			_ ->
					Req1 = http_handler:errormessage_json_response(<<"Файл не передан"/utf8>>, Req0),
					{ok, Req1, State0}
		end
	end, Req0, State0);

process([], #{method := <<"GET">>} = Req0, State0) ->
	http_handler:auth_required(fun(#{ token := Token }) ->
		case get_resource_params(Req0) of
			{ok, ResourceParams} ->
				case yadisk:files(Token, ResourceParams) of
					{ok, #{ <<"items">> := Items } = Resource} ->
						Req1 = http_handler:success_json_response(
							iomod:list_to_json(prepare_images(Items)), Req0),
						{ok, Req1, State0};

					Any ->
						?ERROR("Cant get image list. ~p", [Any]),
						Req1 = http_handler:errormessage_json_response(
							<<"Не удалось получить список изображений"/utf8>>, Req0),
						{ok, Req1, State0}
				end;

			Any ->
				?ERROR("Cant get image list. ~p", [Any]),
				Req1 = http_handler:errormessage_json_response(
					<<"Не удалось получить список изображений"/utf8>>, Req0),
				{ok, Req1, State0}
		end
	end, Req0, State0);

process([<<"preview">>], #{method := <<"GET">>} = Req0, State0) ->
	http_handler:auth_required(fun(#{ token := Token }) ->
		try cowboy_req:match_qs([uri], Req0) of
			#{ uri := Uri } ->
				case yadisk:preview(Token, Uri) of
					{ok, Data} ->
						Req1 = cowboy_req:reply(200, #{}, Data, Req0),
						{ok, Req1, State0};

					Any ->
						?ERROR("Cant download image ~p ~p", [Uri, Any]),
			      Req1 = http_handler:errormessage_json_response(
							<<"Ошибка загрузки изображения"/utf8>>, Req0),
						{ok, Req1, State0}
				end
		catch
			Ex:Exv:Stacktrace ->
	      ?ERROR("Got exception ~p:~p\n~p", [Ex, Exv, Stacktrace]),
	      Req1 = http_handler:errormessage_json_response(
					<<"Адрес изображения не указан"/utf8>>, Req0),
				{ok, Req1, State0}
		end
	end, Req0, State0);

process([<<"origin">>], #{method := <<"GET">>} = Req0, State0) ->
	http_handler:auth_required(fun(#{ token := Token }) ->
		try cowboy_req:match_qs([path], Req0) of
			#{ path := Path } ->
				case yadisk:download(Token, Path) of
					{ok, Data} ->
						Req1 = cowboy_req:reply(200, #{}, Data, Req0),
						{ok, Req1, State0};

					Any -> 
						?ERROR("Cant download image ~p ~p", [Path, Any]),
			      Req1 = http_handler:errormessage_json_response(
							<<"Ошибка загрузки изображения"/utf8>>, Req0),
						{ok, Req1, State0}
				end
		catch
			Ex:Exv:Stacktrace ->
	      ?ERROR("Got exception ~p:~p\n~p", [Ex, Exv, Stacktrace]),
	      Req1 = http_handler:errormessage_json_response(
					<<"Адрес изображения не указан"/utf8>>, Req0),
				{ok, Req1, State0}
		end
	end, Req0, State0);

process(PathInfo, Req0, State0) ->
	?INFO("Unknonwn path info ~p", [PathInfo]),
  {ok, http_handler:not_implemented(Req0), State0}.


 valid_mimetypes() ->
 	[ <<"image/png">>, <<"image/jpeg">> ].


-spec get_resource_params(Req) -> {ok, RequestParams} | {error, term()}
	when
		Req           :: cowboy_req:req(),
		RequestParams :: yadisk:resource_params().
get_resource_params(Req) ->
	Match = 
		[ {limit, [int], 100}
		, {offset, [int], undefined}
		, {sort, [], undefined}
		],
	try cowboy_req:match_qs(Match, Req) of
		#{ limit := Limit, offset := Offset, sort := Sort} ->
			{ok, #{
				path   => <<"app:/">>,
				fields => undefined,
				limit  => Limit,
				offset => Offset,
				preview_crop => <<"true">>,
				preview_size => <<"250x250">>,
				sort   => Sort
			}}
	catch 
    Ex:Exv:Stacktrace ->
    	?ERROR("Got exception ~p:~p\n~p", [Ex, Exv, Stacktrace]),
    	{error, wrong_request}
	end.


prepare_images(Items) ->
	?INFO("~p", [Items]),
	lists:filtermap(
		fun
			(#{ <<"file">> := File, <<"preview">> := Preview}) -> 
				{true, #{ 
					uri => File, 
					preview => preview_image_uri(Preview)
				}};

			(_) ->
				false
		end, 
		Items).

preview_image_uri(Uri) ->
	Qs = cow_qs:qs([{<<"uri">>, Uri}]),
	<<"/api/v1/images/preview?", Qs/binary>>.
