-module( h1 ).
-author( "Warren Kenny <warren.kenny@riotgames.com>" ).
-include_lib( "h1/include/h1.hrl" ).

-export( [init/2, init/3, report/2, reports/2, reports/3, base_url/1, auth/1] ).

-define( DEFAULT_BASE_URL, "https://api.hackerone.com/v1/" ).
-define( DATETIME_FIELDS, [
    created_at, 
    updated_at,
    first_program_activity_at,
    last_activity_at,
    last_program_activity_at,
    last_reporter_activity_at,
    triaged_at,
    closed_at,
    disclosed_at
]).

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
%%  @doc Get the report with the specified ID
%%
-spec report( non_neg_integer(), handle() ) -> { ok, report_response() } | { error, term() }.
report( ID, Handle ) -> 
    case h1_request:get( ["reports", integer_to_list( ID )], Handle ) of
        { ok, Response }    -> { ok, h1_util:to_datetime( Response, ?DATETIME_FIELDS ) };
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
-spec reports( map() | [{ atom(), any() }], handle() ) -> { ok, h1_page:page() } | { error, term() }.
reports( Filters, Handle ) when is_map( Filters ) ->
    reports( maps:to_list( Filters ), Handle );

reports( Filters, Handle ) ->
    case lists:keyfind( program, 1, Filters ) of
        false   ->
            { error, missing_program_filter };
        _ ->
            case h1_request:get( ["reports"], lists:map( fun h1_util:filter_to_param/1, Filters ), Handle ) of
                { ok, Response }    -> { ok, h1_page:init( h1_util:to_datetime( Response, ?DATETIME_FIELDS ), Handle ) };
                { error, Reason }   -> { error, Reason }
            end
    end.

%%
%%  @doc Search for all reports that match the given filters. As the program filter is non-optional, this
%%  function is a convenient way to specify it when using the query endpoint.
%%
-spec reports( map() | [{ atom(), any() }], string(), handle() ) -> { ok, h1_page:page() } | { error, term() }.
reports( Filters, Program, Handle ) when is_map( Filters ) ->
    reports( maps:to_list( Filters ), Program, Handle );

reports( Filters, Program, Handle ) ->
    case lists:keyfind( program, 1, Filters ) of
        false   -> reports( [{ program, [Program] } | Filters], Handle );
        _       -> reports( Filters, Handle )
    end.