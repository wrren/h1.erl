-module( h1_request ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [get/2, get/3, delete/2, delete/3, post/3, post/4] ).

-type request() :: httpc:request().
-type param() 	:: { string(), string() }.
-type params() :: [param()].

%%
%%	Perform an authenticated GET request against the specified API endpoint with the given query parameters.
%%
-spec get( [string()], [ { string(), string() } ], h1:handle() ) -> { error, term() } | { ok, map() }.
get( Endpoint, Params, Handle ) ->
	request( h1:base_url( Handle ), Endpoint, Params, get, undefined, undefined, h1:auth( Handle ) ).

-spec get( [string()], h1:handle() ) -> { error, term() } | { ok, map() }.
get( Endpoint, Handle ) ->
	request( h1:base_url( Handle ), Endpoint, [], get, undefined, undefined, h1:auth( Handle ) ).
	
%%
%%	Perform an authenticated DELETE request against the specified API endpoint with the given query parameters.
%%
-spec delete( [string()], [ { string(), string() } ], h1:handle() ) -> { error, term() } | { ok, map() }.
delete( Endpoint, Params, Handle ) ->
	request( h1:base_url( Handle ), Endpoint, Params, delete, undefined, undefined, h1:auth( Handle ) ).

-spec delete( [string()], h1:handle() ) -> { error, term() } | { ok, map() }.
delete( Endpoint, Handle ) ->
	request( h1:base_url( Handle ), Endpoint, [], delete, undefined, undefined, h1:auth( Handle ) ).

%%
%%	Perform a POST request to the specified API endpoint with the given query parameters and data.
%%
-spec post( [string()], [{ string(), string() }], term(), h1:handle() ) -> { error, term() } | { ok, map() }.
post( Endpoint, Params, Data, Handle ) ->
	request( h1:base_url( Handle ), Endpoint, Params, post, jsx:encode( Data ), "application/json", h1:auth( Handle ) ).
	
%%
%%	Perform an authenticated POST request to the specified API endpoint sending the given data.
%%
-spec post( [string()], term(), h1:handle() ) -> { error, term() } | { ok, map() }.
post( Endpoint, Data, Handle ) ->
    request( h1:base_url( Handle ), Endpoint, [], post, jsx:encode( Data ), "application/json", h1:auth( Handle ) ).

%%
%%	@doc Generate an authentication header for inclusion in a request based on the authentication method
%%	chosen during the gh:init phase
%%
-spec auth( atom() | { atom(), string() } | { atom(), string(), string() }, [{string(), string()}] ) -> [{ string(), string() }].
auth( anonymous, Headers ) -> 
	Headers;

auth( { oauth, Token }, Headers ) ->
	[{ "Authorization", string:concat( "token ", Token ) } | Headers];
	
auth( { basic, Username, Password }, Headers ) ->
	BasicAuth = base64:encode_to_string( lists:append( [ Username, ":", Password ] ) ),
    [{ "Authorization", string:concat( "Basic ", BasicAuth ) } | Headers].


%%
%%	@doc Generate a URL by joining a base URL with the provided path and query parameters
%%
-spec url( string() | binary(), string(), [{ string(), string() }] ) -> string().
url( Url, Path, QueryParams ) when is_list( Url ) ->
	case lists:last( Url ) of
		$/	-> 	url( lists:droplast( Url ), Path, QueryParams );
		_	->	url( list_to_binary( Url ), Path, QueryParams )
	end;

url( <<Url, $/>>, Path, QueryParams ) ->
	url( Url, Path, QueryParams );

url( Url, Path, QueryParams ) ->
	binary_to_list( iolist_to_binary( lists:droplast( lists:flatten( [ 	Url, 
							[ [ $/, P ] || P <- Path ], 
							$?, 
[ [ K, $=, V, $& ] || { K, V } <- QueryParams ] ] ) ) ) ).

%%
%%	@doc Make a request to the HackerOne API
%%
-spec request( atom(), request(), string() ) -> { ok, term() } | { error, term() }.
request( Method, Request, Url ) ->
	case httpc:request( Method, setelement( 1, Request, Url ), [], [{ body_format, binary }] ) of
		%% Request completed with no content returned
		{ ok, { { _Version, 204, _Reason }, _Headers, _Body } } ->
			{ ok, no_content };
		
		{ ok, { { _Version, Status, _Reason }, _Headers, Body } } when Status >= 200 andalso Status < 300 ->
			{ ok, jsx:decode( Body, [ { labels, atom }, return_maps ] ) };
		%% Some other status code
		{ ok, { { _Version, Status, Reason }, _Headers, Body } } ->
			{ error, { Status, Reason, Body } };
		%% Request failed
		{ error, Reason } -> 
			{ error, Reason }
	end.

-spec request( string(), [string()], params(), atom(), term(), string(), h1:handle() ) -> { ok, term() } | { error, term() }.
request( BaseURL, Endpoint, Params, Method, Body, ContentType, Auth ) when 		Method =:= post 	orelse 
																			    Method =:= put 		orelse 
																				Method =:= patch ->
	Url = url( BaseURL, Endpoint, Params ),
	request( Method, { Url, auth( Auth, [] ), ContentType, Body }, Url );

request( BaseURL, Endpoint, Params, Method, _Body, _ContentType, Auth ) when 	Method =:= get 		orelse 
																		        Method =:= delete 	orelse 
																		        Method =:= head 	orelse 
																		        Method =:= options ->
	Url = url( BaseURL, Endpoint, Params ),
request( Method, { Url, auth( Auth, [] ) }, Url ).