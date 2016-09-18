-module( h1_util_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

to_datetime_test() ->
    ?assertMatch( null, h1_util:to_datetime( <<"null">> ) ),
    ?assertMatch( { { 2016, 09, 17 }, { 07, 04, 24 } }, h1_util:to_datetime( <<"2016-09-17T07:04:24.635Z">> ) ),
    ?assertMatch( { { 2016, 09, 17 }, { 07, 04, 24 } }, h1_util:to_datetime( <<"2016-09-17T07:04:24Z">> ) ).

filter_param_test() ->
    ?assertMatch( { "filter[last_activity_at__gt]", _ }, h1_util:filter_to_param( { last_activity_after, calendar:universal_time() } ) ).