-module( h1 ).
-author( "Warren Kenny <warren.kenny@riotgames.com>" ).
-include_lib( "h1/include/h1.hrl" ).

-export( [init/2, init/3, report/2, reports/2, reports/3, base_url/1, auth/1, to_datetime/1] ).

-define( DEFAULT_BASE_URL, "https://api.hackerone.com/v1/" ).

%%
%%  h1 handle
%%
-record( handle, {  auth :: anonymous | { oauth, Token :: string() } | { basic, Username :: string(), Password :: string() }, 
                    base_url :: string() } ).

-type handle()  :: #handle{}.
-export_type( [handle/0] ).

-define( SEARCH_FILTERS, [
    { program, list,                        "program" },
    { state, list,                          "state" },
    { id, list,                             "id" },
    { created_after,                        "created_at__gt" }, 
    { created_before,                       "created_at__lt" },
    { triaged_after,                        "triaged_at__gt" },
    { triaged_before,                       "triaged_at__lt" },
    { closed_after,                         "closed_at__gt" },
    { closed_before,                        "closed_at__lt" },
    { disclosed_after,                      "disclosed_at__gt" },
    { disclosed_before,                     "disclosed_at__lt" },
    { swag_awarded_after,                   "swag_awarded_at__gt" },
    { swag_awarded_before,                  "swag_awarded_at__lt" },
    { bounty_awarded_after,                 "bounty_awarded_at__gt" },
    { bounty_awarded_before,                "bounty_awarded_at__lt" },
    { last_reporter_activity_after,         "last_reporter_activity_at__gt" },
    { last_reporter_activity_before,        "last_reporter_activity_at__lt" },
    { last_program_activity_after,          "last_program_activity_at__gt" },
    { last_program_activity_before,         "last_program_activity_at__lt" },
    { first_program_activity_after,         "first_program_activity_at__gt" },
    { first_program_activity_before,        "first_program_activity_at__lt" },
    { last_activity_after,                  "last_activity_at__gt" },
    { last_activity_before,                 "last_activity_at__lt" },
    { triaged,                              "triaged_at__null" },
    { closed,                               "closed_at__null" },
    { disclosed,                            "disclosed_at__null" },
    { swag_awarded,                         "swag_awarded_at__null" },
    { bounty_awarded,                       "bounty_awarded_at__null" },
    { first_program_activity,               "first_program_activity_at__null" }
] ).

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
%%  @doc Convert a filter tuple in the form { atom(), any() } into a query parameter pair by
%%  matching the filter name against the SEARCH_FILTERS proplist.
%%
filter_to_param( { Filter, Value } ) ->
    case lists:keyfind( Filter, 1, ?SEARCH_FILTERS ) of
        false           -> erlang:error( io_lib:format( "Unrecognized Filter Key ~p", [ Filter ] ) );
        F               -> filter_to_param( F, Value )
    end.

%%
%%  @doc Perform any type conversion necessary to turn the filter value into a parameter value
%%
filter_to_param( { Key, _ }, DateTime = { _Date, _Time } )              -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), ec_date:format_iso8601( DateTime ) };
filter_to_param( { Key, _ }, true )                                     -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), "false" };
filter_to_param( { Key, _ }, false )                                    -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), "true" };
filter_to_param( { Key, _ }, Value ) when is_binary( Value )            -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), binary_to_list( Value ) };
filter_to_param( { Key, _ }, Value )                                    -> 
    { lists:flatten( io_lib:format( "filter[~s][]", [Key] ) ), want:string( Value ) };
filter_to_param( { Key, list, _ }, [Value] ) when is_list( Value )      ->
    { lists:flatten( io_lib:format( "filter[~s][]", [Key] ) ), Value };
filter_to_param( { Key, list, _ }, Values ) when is_list( Values )      ->
    { lists:flatten( io_lib:format( "filter[~s][]", [Key] ) ), string:join( [ want:string( V ) || V <- Values ], "," ) }.

%%
%%  @doc Get the report with the specified ID
%%
-spec report( non_neg_integer(), handle() ) -> { ok, report_response() } | { error, term() }.
report( ID, Handle ) -> 
    case h1_request:get( ["reports", integer_to_list( ID )], Handle ) of
        { ok, Response }    -> { ok, to_datetime( Response, [created_at, updated_at] ) };
        { error, Reason }   -> { error, Reason }
    end.

%%
%%  @doc Search for all reports that match the given filters. Possible
%%  filter keys and expected types:
%%
%%     program ([string()] | string())
%%     state ([string()] | string())
%%     id ([string()] | string())
%%     created_after (calendar:datetime() | string()) 
%%     created_before (calendar:datetime() | string())
%%     triaged_after (calendar:datetime() | string())
%%     triaged_before (calendar:datetime() | string())
%%     closed_after (calendar:datetime() | string())
%%     closed_before (calendar:datetime() | string())
%%     disclosed_after (calendar:datetime() | string())
%%     disclosed_before (calendar:datetime() | string())
%%     swag_awarded_after (calendar:datetime() | string())
%%     swag_awarded_before (calendar:datetime() | string())
%%     bounty_awarded_after (calendar:datetime() | string())
%%     bounty_awarded_before (calendar:datetime() | string())
%%     last_reporter_activity_after (calendar:datetime() | string())
%%     last_reporter_activity_before (calendar:datetime() | string())
%%     last_program_activity_after (calendar:datetime() | string())
%%     last_program_activity_before (calendar:datetime() | string())
%%     first_program_activity_after (calendar:datetime() | string())
%%     first_program_activity_before (calendar:datetime() | string())
%%     last_activity_after (calendar:datetime() | string())
%%     last_activity_before (calendar:datetime() | string())
%%     triaged (boolean())
%%     closed (boolean())        
%%     disclosed (boolean())       
%%     swag_awarded (boolean())        
%%     bounty_awarded (boolean())        
%%     first_program_activity (boolean())       
%%
-spec reports( map() | [{ atom(), any() }], handle() ) -> { ok, map() } | { error, term() }.
reports( Filters, Handle ) when is_map( Filters ) ->
    reports( maps:to_list( Filters ), Handle );

reports( Filters, Handle ) ->
    case lists:keyfind( program, 1, Filters ) of
        false   ->
            { error, missing_program_filter };
        _ ->
            case h1_request:get( ["reports"], lists:map( fun filter_to_param/1, Filters ), Handle ) of
                { ok, Response }    -> { ok, to_datetime( Response, [created_at, updated_at] ) };
                { error, Reason }   -> { error, Reason }
            end
    end.

%%
%%  @doc Search for all reports that match the given filters. As the program filter is non-optional, this
%%  function is a convenient way to specify it when using the query endpoint.
%%
-spec reports( map() | [{ atom(), any() }], string(), handle() ) -> { ok, map() } | { error, term() }.
reports( Filters, Program, Handle ) when is_map( Filters ) ->
    reports( maps:to_list( Filters ), Program, Handle );

reports( Filters, Program, Handle ) ->
    case lists:keyfind( program, 1, Filters ) of
        false   -> reports( [{ program, [Program] } | Filters], Handle );
        _       -> reports( Filters, Handle )
    end.