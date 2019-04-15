-module(http_body).
-export([
	body/1,
	json_body/1,
	form_multipart_body/1,
	form_urlencoded_body/1,

	default_content_type/0,

	request_body/1
	]).

-include("wa.hrl").
-include("http_body.hrl").


-spec body(cowboy_req:req()) -> {ok, http_body(), cowboy_req:req()} | {error, term()}.
body(Req0) ->
	case cowboy_req:parse_header(<<"content-type">>, Req0, default_content_type()) of
		{<<"application">>, <<"json">>, _} ->
			json_body(Req0);
		{<<"multipart">>, <<"form-data">>, _} ->
			form_multipart_body(Req0);
		{<<"application">>, <<"x-www-form-urlencoded">>, _} ->
			form_urlencoded_body(Req0);
		Ct ->
			{error, {unknown_ct, Ct}}
	end.


-spec json_body(cowboy_req:req()) -> {ok, http_body(), cowboy_req:req()} | {error, term()}.
json_body(Req0) ->
	{ok, Body, Req} = request_body(Req0),
	try ?FROM_JSON(Body) of
		Json -> {ok, Json, Req}
	catch
		Ex:Exv:Stacktrace ->
      ?ERROR("Cant parse json body ~p:~p\n~w", [Ex, Exv, Stacktrace]),
			{error, not_json}
	end.


-spec form_multipart_body(cowboy_req:req()) -> {ok, http_body(), cowboy_req:req()} | {error, term()}.
form_multipart_body(Req0) ->
	request_multipart_body(Req0).


-spec form_urlencoded_body(cowboy_req:req()) -> {ok, http_body(), cowboy_req:req()}.
form_urlencoded_body(Req0) ->
	{ok, Data, Req} = cowboy_req:read_urlencoded_body(Req0),
	{ok, maps:from_list(Data), Req}.


-spec default_content_type() -> http_body_content_type().
default_content_type() ->
	{<<"application">>, <<"json">>, #{}}.


-spec request_body(cowboy_req:req()) -> {ok, binary(), cowboy_req:req()}.
request_body(Req0) ->
	case cowboy_req:has_body(Req0) of
		true  -> request_body(Req0, <<>>);
		false -> <<>>
	end.

-spec request_body(cowboy_req:req(), binary()) -> {ok, binary(), cowboy_req:req()}.
request_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok,   Data, Req}   -> {ok, <<Acc/binary, Data/binary>>, Req};
    {more, Data, Req} -> request_body(Req, <<Acc/binary, Data/binary>>)
  end.


-spec request_multipart_body(cowboy_req:req()) -> {ok, http_body(), cowboy_req:req()}.
request_multipart_body(Req0) -> 
  request_multipart_body(Req0, #{}).

-spec request_multipart_body(cowboy_req:req(), http_body()) -> {ok, http_body(), cowboy_req:req()}.
request_multipart_body(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
      {ok, Headers, Req} ->
        case cow_multipart:form_data(Headers) of
          {data, FieldName} ->
            {Value, Req1} = part_body(Req),
            request_multipart_body(Req1, maps:put(FieldName, Value, Acc));
           {file, FieldName, Filename, CType} ->
            {FileBinary, Req1} = part_body(Req),
            request_multipart_body(
              Req1,
             	maps:put(FieldName, 
	              #{
	                filename => Filename, 
	                data => FileBinary, 
	                content_type => CType, 
	                encoding => undefined
	              }, Acc));
          {file, FieldName, Filename, CType, CTransferEncoding} ->
            {FileBinary, Req1} = part_body(Req),
            request_multipart_body(
              Req1,
             	maps:put(FieldName, 
	              #{
	                filename => Filename, 
	                data => FileBinary, 
	                content_type => CType, 
	                encoding => CTransferEncoding
	              }, Acc))
        end;
      {done, Req} ->
          {ok, Acc, Req}
    end.


-spec part_body(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
part_body(Req0) ->
	part_body(Req0, <<>>).

-spec part_body(cowboy_req:req(), binary()) -> {binary(), cowboy_req:req()}.
part_body(Req0, Acc) ->
	case cowboy_req:read_part_body(Req0) of
        {ok, Body, Req} ->
            {<<Acc/binary, Body/binary>>, Req};
        {more, Body, Req} ->
            part_body(Req, <<Acc/binary, Body/binary>>)
    end.