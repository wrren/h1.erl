-module( h1_util ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "h1/include/h1.hrl" ).

-export( [filter_to_param/1, to_datetime/1, to_datetime/2] ).

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
filter_to_param( { _, Key }, DateTime = { _Date, _Time } )              -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), ec_date:format_iso8601( DateTime ) };
filter_to_param( { _, Key }, true )                                     -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), "false" };
filter_to_param( { _, Key }, false )                                    -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), "true" };
filter_to_param( { _, Key }, Value ) when is_binary( Value )            -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), binary_to_list( Value ) };
filter_to_param( { _, Key }, Value )                                    -> 
    { lists:flatten( io_lib:format( "filter[~s]", [Key] ) ), want:string( Value ) };
filter_to_param( { _, list, Key }, [Value] ) when is_list( Value )      ->
    { lists:flatten( io_lib:format( "filter[~s][]", [Key] ) ), Value };
filter_to_param( { _, list, Key }, Values ) when is_list( Values )      ->
    { lists:flatten( io_lib:format( "filter[~s][]", [Key] ) ), string:join( [ want:string( V ) || V <- Values ], "," ) }.