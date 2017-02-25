-type datetime() :: null | calendar:datetime().

-type user()            ::  #{  username        => binary(),
                                name            => binary(),
                                disabled        => boolean(),
                                created_at      => datetime(),
                                profile_picture => #{
                                  '62x62'     => binary(),
                                  '82x82'     => binary(),
                                  '110x110'   => binary(),
                                  '260x260'   => binary() 
                                } 
}.

-type group()           ::  #{  name            => binary(),
                                created_at      => datetime() 
}.

-type program()         ::  #{  handle          => binary(),
                                created_at      => datetime(),
                                updated_at      => datetime() 
}.

-type bounty()          ::  #{  amount          => float(),
                                bonus_amount    => float(),
                                created_at      => datetime() 
}.

-type vulnerability()   ::  #{  name            => binary(),
                                description     => binary(),
                                created_at      => datetime() 
}.

-type attachment()      ::  #{  file_name       => binary(),
                                content_type    => binary(),
                                file_size       => integer(),
                                expiring_url    => binary(),
                                created_at      => datetime() 
}.                        

-type summary()         ::  #{  content         => binary(),
                                category        => [binary()],
                                created_at      => datetime(),
                                updated_at      => datetime()
}.

-type swag()            ::  #{  sent            => boolean(),
                                created_at      => datetime() 
}.

-type address()         ::  #{  name            => binary(),
                                street          => binary(),
                                city            => binary(),
                                postal_code     => binary(),
                                state           => binary(),
                                country         => binary(),
                                tshirt_size     => binary(),
                                phone_number    => binary(),
                                created_at      => datetime() 
}.

-type activity()        :: #{   message         => binary(),
                                internal        => boolean(),
                                created_at      => datetime(),
                                updated_at      => datetime() }.

-type report()          :: #{   title                       => binary(),
                                vulnerability_information   => binary(),
                                state                       => binary(),
                                created_at                  => datetime(),
                                triaged_at                  => datetime(),
                                closed_at                   => datetime(),
                                last_reporter_activity_at   => datetime(),
                                first_program_activity_at   => datetime(),
                                last_program_activity_at    => datetime(),
                                last_activity_at            => datetime(),
                                bounty_awarded_at           => datetime(),
                                swag_awarded_at             => datetime(),
                                disclosed_at                => datetime(),
                                issue_tracker_reference_id  => binary(),
                                issue_tracker_reference_url => binary() 
}.

-type object( Attr )        :: #{   id              => binary(),
                                    type            => binary(),
                                    attributes      => Attr
}.

-type object( Attr, Rel )   :: #{   id              => binary(),
                                    type            => binary(),
                                    attributes      => Attr,
                                    relationships   => Rel 
}.

-type report_response() :: object( report(), #{ 
    reporter                => user(),
    program                 => object( program() ),
    swag                    => [object( swag(), #{ address => address() } )],
    attachments             => [object( attachment() )],
    vulnerability_types     => [object( vulnerability() )],
    reporter                => object( user() ),
    activities              => [object( activity(), #{ actor => user() | program(), attachments => [object( attachment() )] } )],
    bounties                => [object( bounty() )],
    summaries               => [object( summary(), #{ user => user() } )] } 
).