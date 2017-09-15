-module( h1_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-compile( export_all ).
-include_lib( "common_test/include/ct.hrl" ).

all() -> [report_test, reports_test, pagination_test].

init_per_suite( Config ) ->
    [{ handle, h1:init( ct:get_config( h1_id ), ct:get_config( h1_key ) ) } | Config].

report_test( Config ) ->
    { ok, _ } = h1:report( ct:get_config( h1_report_id ), ?config( handle, Config ) ).

reports_test( Config ) ->
    { ok, _ } = h1:reports( [{ bounty_awarded, true }, { created_before, "2016-09-17T07:05:58.602Z" }], ct:get_config( h1_program ), ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ bounty_awarded, true }, { created_before, calendar:universal_time() }], [ct:get_config( h1_program )], ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }], [ct:get_config( h1_program )], ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }, { program, [ct:get_config( h1_program )] }], ?config( handle, Config ) ),
    { ok, _ } = h1:reports( [{ created_before, "2016-09-16T00:00:00Z" }, { bounty_awarded, true }], ct:get_config( h1_program ), ?config( handle, Config ) ),
    { ok, _ } = h1:reports( #{  created_before => "2016-09-16T00:00:00Z",
                                bounty_awarded => true }, ct:get_config( h1_program ), ?config( handle, Config ) ),
    { ok, _ } = h1:reports( #{  closed_before => calendar:universal_time(), 
                                bounty_awarded => true }, ct:get_config( h1_program ), ?config( handle, Config ) ),
    { ok, _ } = h1:reports( #{  last_activity_after => "2015-09-16T00:00:00Z", 
                                bounty_awarded => true }, ct:get_config( h1_program ), ?config( handle, Config ) ),
    { ok, _ } = h1:reports( #{  last_activity_before => calendar:universal_time(), 
                                bounty_awarded => true }, ct:get_config( h1_program ), ?config( handle, Config ) ),
    { error, missing_program_filter } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }], ?config( handle, Config ) ).

pagination_test( Config ) ->
    { ok, Page } = h1:reports( [], ct:get_config( h1_program ), ?config( handle, Config ) ),
    case h1_page:is_last( Page ) of
        true    -> 
            ok;
        false   ->
            { ok, NextPage } = h1_page:next( Page ),
            2 = h1_page:number( NextPage )
    end,
    { ok, OnePage } = h1:reports( [{ id, [ct:get_config( h1_report_id )] }], [ct:get_config( h1_program )], ?config( handle, Config ) ),
    true = h1_page:is_last( OnePage ),
    1 = h1_page:number( OnePage ).

end_per_suite( Config ) ->
    Config.