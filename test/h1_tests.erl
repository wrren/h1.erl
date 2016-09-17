-module( h1_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

to_datetime_test() ->
    ?assertMatch( null, h1:to_datetime( <<"null">> ) ),
    ?assertMatch( { { 2016, 09, 17 }, { 07, 04, 24 } }, h1:to_datetime( <<"2016-09-17T07:04:24.635Z">> ) ),
    ?assertMatch( { { 2016, 09, 17 }, { 07, 04, 24 } }, h1:to_datetime( <<"2016-09-17T07:04:24Z">> ) ).