%%
%%  @author Warren Kenny
%%  @doc Provides functions that simplify interactions with result pages from the HackerOne API
%%
-module( h1_page ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "h1/include/h1.hrl" ).

-ifdef( TEST ).
-compile( export_all ).
-endif.

-export( [init/2, number/1, reports/1, is_last/1, count/1, next/1] ).

-record( page, {    self        :: binary(),
                    next        :: binary(),
                    number      :: non_neg_integer(),
                    count       :: non_neg_integer(),
                    is_last     :: boolean(),
                    handle      :: h1:handle(),
                    reports     :: [report_response()]
} ).

-type page() :: #page{}.
-export_type( [page/0] ).

%%
%%  @doc Determine the page number indicated by the specified URL by parsing the page[number] field.
%%  Returns 0 on error.
%%
-spec page_number( binary() | [string()] ) -> non_neg_integer().
page_number( Url ) when is_binary( Url ) ->
    Decoded = want:string( url:decode( Url ) ),
    case string:rchr( Decoded, $? ) of
        0 -> 0;
        N -> page_number( string:tokens( string:substr( Decoded, N ), "&" ) )
    end;

page_number( Params ) when is_list( Params ) ->
    case lists:filtermap( fun( P ) ->
        case string:tokens( P, "=" ) of 
            ["page[number]", Number] -> { true, want:integer( Number ) };
            _                           -> false
        end end, Params ) of
        [Number | _]    -> Number;
        _               -> 0
    end.    

%%
%%  @doc Initialize a new page handle using a response map received from the reports query endpoint
%%
-spec init( map(), h1:handle() ) -> page().
init( #{ data := Reports, links := Links = #{ self := _Self } }, Handle ) ->
    #page{  self        = maps:get( self, Links, undefined ),
            next        = maps:get( next, Links, undefined ),
            number      = page_number( maps:get( self, Links ) ),
            count       = page_number( maps:get( last, Links, <<"">> ) ),
            is_last     = maps:is_key( last, Links ) =:= false,
            handle  = Handle,
            reports = Reports };

init( #{ data := Reports, links := #{} }, Handle ) ->
    #page{  self        = undefined,
            next        = undefined,
            number      = 1,
            count       = 1,
            is_last     = true,
            handle      = Handle,
            reports     = Reports }.

%%
%%  @doc Get the page number of the current page. Returns 0 if the page number couldn't be determined
%%
-spec number( page() ) -> non_neg_integer().
number( #page{ number = N } ) -> N.

%%
%%  @doc Get the total number of pages contained in the result set
%%
-spec count( page() ) -> non_neg_integer().
count( #page{ count = C } ) -> C.

%%
%%  @doc Get the list of reports provided by this page
%%
-spec reports( page() ) -> [report_response()].
reports( #page{ reports = Reports } ) -> Reports.

%%
%%  @doc Indicates whether this is the last page in the result set
%%
-spec is_last( page() ) -> boolean().
is_last( Page ) -> Page#page.is_last.

%%
%%  @doc Request the next page in the result set
%%
-spec next( page() ) -> { ok, page() } | { error, term() }.
next( #page{ next = undefined } ) ->
    { error, last_page };
next( #page{ next = Next, handle = Handle } ) ->
    case h1_request:get( Next, Handle ) of
        { ok, Response }    -> { ok, init( h1_util:to_datetime( Response, h1:datetime_fields() ), Handle ) };
        { error, Reason }   -> { error, Reason }
    end.