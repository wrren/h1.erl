-module( h1 ).
-author( "Warren Kenny <warren.kenny@riotgames.com>" ).
-include_lib( "h1/include/h1.hrl" ).

-export( [init/2, init/3, report/2, base_url/1, auth/1, to_datetime/1] ).

-define( DEFAULT_BASE_URL, "https://api.hackerone.com/v1/" ).

%%
%%  h1 handle
%%
-record( handle, {  auth :: anonymous | { oauth, Token :: string() } | { basic, Username :: string(), Password :: string() }, 
                    base_url :: string() } ).

-type handle()  :: #handle{}.
-export_type( [handle/0] ).

%%
%%  @doc Get the API Base URL from the given handle
%%
base_url( Handle )  -> Handle#handle.base_url.

%%
%%  @doc Get the authentication tuple from the given handle
%%
auth( Handle )  -> Handle#handle.auth.

%%
%%  @doc Initialize a new h1 handle. The h1 handle is used in all h1 functions that interact
%%  with the HackerOne API. Provide the Token ID and Key retrieved from your API settings page.
%%
-spec init( string(), string() ) -> handle().
init( ID, Key ) ->
    init( ID, Key, ?DEFAULT_BASE_URL ).

%%
%%  @doc Initialize a new h1 handle. The h1 handle is used in all h1 functions that interact
%%  with the HackerOne API. Provide the Token ID and Key retrieved from your API settings page as 
%%  well as an API base URL. The Base URL should only be provided if you're accessing a HackerOne API
%%  from a server other than api.hackerone.com
%%
-spec init( string(), string(), string() ) -> handle().
init( ID, Key, BaseURL ) ->
    #handle{ auth = { basic, ID, Key }, base_url = BaseURL }.

%%
%%  @doc Convert a value representing an ISO8601 datetime with milliseconds to an erlang calendar:datetime() term.
%%
-spec to_datetime( binary() ) -> datetime().
to_datetime( <<"null">> ) -> 
    null;
to_datetime( DateTime ) when is_binary( DateTime ) -> 
    to_datetime( binary_to_list( DateTime ) );
to_datetime( DateTime ) when is_list( DateTime ) -> 
    case string:rchr( DateTime, $. ) of
        0   -> to_datetime( ec_date:parse( DateTime ) );
        N   -> to_datetime( ec_date:parse( string:concat( string:substr( DateTime, 1, N - 1 ), "Z" ) ) )
    end;
to_datetime( { Date, { H, M, S, _ } } ) -> { Date, { H, M, S } }.

%%
%%  @doc Traverse the given map and convert any keys matching those provided to calendar:datetime() terms
%%
-spec to_datetime( map(), [atom()] ) -> map().
to_datetime( Map, Keys ) ->
    maps:from_list( lists:map( fun( { Key, Value } ) ->
        case { lists:member( Key, Keys ), is_map( Value ) } of
            { _, true } -> { Key, to_datetime( Value, Keys ) };
            { true, _ } -> { Key, to_datetime( Value ) };
            _           -> { Key, Value }
        end
    end, maps:to_list( Map ) ) ).

%%
%%  @doc Get the report with the specified ID
%%
-spec report( non_neg_integer(), handle() ) -> { ok, report_response() } | { error, term() }.
report( ID, Handle ) -> 
    case h1_request:get( ["reports", integer_to_list( ID )], Handle ) of
        { ok, Response }    -> { ok, to_datetime( Response, [created_at, updated_at] ) };
        { error, Reason }   -> { error, Reason }
    end.