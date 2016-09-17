-module( h1_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-compile( export_all ).
-include_lib( "common_test/include/ct.hrl" ).

all() -> [report_test].

report_test( _Config ) ->
    Handle = h1:init( ct:get_config( h1_id ), ct:get_config( h1_key ) ),
    { ok, _ } = h1:report( ct:get_config( h1_report_id ), Handle ).