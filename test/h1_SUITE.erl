-module( h1_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-compile( export_all ).
-include_lib( "common_test/include/ct.hrl" ).

all() -> [report_test, reports_test].

init_per_suite( Config ) ->
    [{ handle, h1:init( ct:get_config( h1_id ), ct:get_config( h1_key ) ) } | Config].

report_test( Config ) ->
    { ok, _ } = h1:report( ct:get_config( h1_report_id ), ?config( handle, Config ) ).

reports_test( Config ) ->
    { ok, _ } = h1:reports( [{ bounty_awarded, true }, { created_before, "2016-09-17T07:05:58.602Z" }], [ct:get_config( h1_program )], ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ bounty_awarded, true }, { created_before, calendar:universal_time() }], [ct:get_config( h1_program )], ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }], [ct:get_config( h1_program )], ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }, { program, [ct:get_config( h1_program )] }], ?config( handle, Config ) ),
    { error, missing_program_filter } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }], ?config( handle, Config ) ).