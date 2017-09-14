%%------------------------------------------------------------------------------
%% hal module
%%
%% A very simple module for creating hal like records based on the
%% Hal Specification(s)
%%
%% - http://www.erlang.se/doc/programming_rules.shtml#REF14536
%% - https://tools.ietf.org/html/draft-kelly-json-hal-08
%%
%% Various relevant specifications
%% - https://tools.ietf.org/html/rfc4627 
%% - https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5
%% - https://www.iana.org/assignments/media-types/media-types.xhtml
%% - https://tools.ietf.org/html/rfc6838
%% - https://tools.ietf.org/html/rfc4855
%% - https://tools.ietf.org/html/draft-wilde-profile-link-04
%% - https://tools.ietf.org/html/rfc5988
%% - https://tools.ietf.org/html/draft-kelly-json-hal-08#ref-I-D.wilde-profile-link
%% 
%% Programming Rules for this module 
%% - http://www.erlang.se/doc/programming_rules.shtml#REF14536 
%%------------------------------------------------------------------------------
-module(hal).
-export([create_hal/0, create_hal/1, create_hal/2]).
-export([create_link/1, create_link/2, create_link/3, create_link/4, 
         create_link/5, create_link/6, create_link/7, create_link/8]).
-export([add_link/2, add_link/3]).
-export([add_links/2, add_links/3]). 
-export([add_state/2, add_state/3]).
-export([add_embedded/3]).
-export([create_curie/1, create_curie/2]).
-export([add_curie/2]).
-export([hal_to_map/1, 
         state_to_map/1, 
         links_to_map/1, 
         link_to_map/1, 
         embedded_to_map/1, 
         curie_to_map/1, curies_to_map/1]).
-export([test/0]). 

%% eunit testing
-include_lib("eunit/include/eunit.hrl").


%%------------------------------------------------------------------------------
%% Data Type: hal
%% where:
%%     links:    A map (default is empty)
%%     embedded: A map (default is empty)
%%     state:    A map (default is empty)
%%     curies:   A list (default is empty)
%%------------------------------------------------------------------------------
-record(hal, {links = maps:new(), embedded = maps:new(), state = maps:new(), curies = []}).

%%------------------------------------------------------------------------------
%% Data Type: link
%% where
%%     href:        A bitstring 
%%     templated:   A boolean
%%     type:        A bitstring
%%     deprecation: A bitstring A url that should provide further information
%%     name:        A bitstring
%%     profile:     A bitstring
%%     title:       A bitstring
%%     hreflang:    A bitstring
%%------------------------------------------------------------------------------
-record(link, {href, templated, type, deprecation, name, profile, title, hreflang}).

%%------------------------------------------------------------------------------
%% Data Type: curie
%% where:
%%     name: A bitstring
%%     href: A bitstring
%%     templated: A boolean
%%------------------------------------------------------------------------------
-record(curie, {name, href, templated = true}).

%%------------------------------------------------------------------------------
%% Various const definitions used by the hal module
%%------------------------------------------------------------------------------
-define(SELF,        <<"self">>).
-define(LINKS,       <<"_links">>).
-define(EMBEDDED,    <<"_embedded">>).
-define(HREF,        <<"href">>).
-define(TEMPLATED,   <<"templated">>).
-define(TYPE,        <<"type">>).
-define(DEPRECATION, <<"deprecation">>).
-define(NAME,        <<"name">>).
-define(PROFILE,     <<"profile">>).
-define(TITLE,       <<"title">>).
-define(HREFLANG,    <<"hreflang">>).
-define(CURIES,      <<"curies">>).

%%------------------------------------------------------------------------------
%% Function: create_hal/0
%% Purpose:  Create a new empty hal record
%% Returns:  A hal record
%% -----------------------------------------------------------------------------
create_hal() -> 
    #hal{}.

%%------------------------------------------------------------------------------
%% Function: create_hal/1
%% Purpose:  Create a new hal record with the supplied self link
%% Args:     Self is a link record
%% Returns:  A hal record
%%------------------------------------------------------------------------------
create_hal(Self) when erlang:is_record(Self, link) ->
    hal:add_link(#hal{}, ?SELF, Self);

%%------------------------------------------------------------------------------
%% Function: create_hal/1
%% Purpose:  Create a new hal record with the supplied self link
%% Args:     Self is a bitstring
%% Returns:  A hal record
%%------------------------------------------------------------------------------
create_hal(Self) when erlang:is_bitstring(Self) ->
    hal:create_hal(hal:create_link(Self)).

%%------------------------------------------------------------------------------
%% Function: create_hal/2
%% Purpose:  Create a hal record with the supplied Self link value and a map of 
%%           key value pairs
%% Args:     Self is a bitstring
%%           State is a map of state values
%% Returns:  A hal record
%%------------------------------------------------------------------------------
create_hal(Self, State) ->
    Hal = hal:create_hal(Self),
    hal:add_state(Hal, State).

%%------------------------------------------------------------------------------
%% Function: create_link/1
%% Purpose:  Create a new link record using the supplied href value
%% Args:     Href is a bitstring or a link record.
%%           Its value is either a URI [RFC3986] or a URI Template [RFC6570].
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href) when erlang:is_bitstring(Href) -> 
    #link{href = Href};
create_link(Link) when erlang:is_record(Link, link) ->
    Link.

%%------------------------------------------------------------------------------
%% Function: create_link/2
%% Purpose:  Create a new link record using the supplied href and templated values
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated) when erlang:is_boolean(Templated) -> 
   Link = hal:create_link(Href),
   Link#link{templated = Templated}. 

%%------------------------------------------------------------------------------
%% Function: create_link/3
%% Purpose:  Create a new link record using the supplied href, templated and 
%%           type  values
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%%           Type is a mime type
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated, Type) -> 
    Link = hal:create_link(Href, Templated),
    Link#link{type = Type}.

%%------------------------------------------------------------------------------
%% Function: create_link/4
%% Purpose:  Create a new link record using the supplied href, templated, type
%%           and deprecation values 
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%%           Type is a mime type
%%           Deprecation should be a url
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated, Type, Deprecation) -> 
    Link = hal:create_link(Href, Templated, Type),
    Link#link{deprecation = Deprecation}.

%%------------------------------------------------------------------------------
%% Function: create_link/5
%% Purpose:  Create a new link record using the supplied href, templated, type
%%           deprecation and name  values 
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%%           Type is a mime type
%%           Deprecation should be a url
%%           Name should be a string/bitstring
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated, Type, Deprecation, Name) -> 
    Link  = hal:create_link(Href, Templated, Type, Deprecation),
    Link#link{name = Name}.

%%------------------------------------------------------------------------------
%% Function: create_link/6
%% Purpose:  Create a new link record using the supplied href, templated, type
%%           deprecation, name and profile  values 
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%%           Type is a mime type
%%           Deprecation should be a url
%%           Name should be a string/bitstring
%%           Proflie should be an I-D.wilde-profile-link
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated, Type, Deprecation, Name, Profile) -> 
    Link = hal:create_link(Href, Templated, Type, Deprecation, Name),
    Link#link{profile = Profile}.

%%------------------------------------------------------------------------------
%% Function: create_link/7
%% Purpose:  Create a new link record using the supplied href, templated, type
%%           deprecation, name and profile  values 
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%%           Type is a mime type
%%           Deprecation should be a url
%%           Name should be a string/bitstring
%%           Proflie should be an I-D.wilde-profile-link
%%           Title should be a string/bitstring
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated, Type, Deprecation, Name, Profile, Title) -> 
    Link = hal:create_link(Href, Templated, Type, Deprecation, Name, Profile),
    Link#link{title = Title}.

%%------------------------------------------------------------------------------
%% Function: create_link/8
%% Purpose:  Create a new link record using the supplied href, templated, type
%%           deprecation, name, profile, title and hreflang  values 
%% Args:     Href is a bitstring or a link record.
%%           Templated is a boolean (true|false)
%%           Type is a mime type
%%           Deprecation should be a url
%%           Name should be a string/bitstring
%%           Proflie should be an I-D.wilde-profile-link
%%           Title should be a string/bitstring
%%           HrefLang should be a bitstring
%% Returns:  A link record
%%------------------------------------------------------------------------------
create_link(Href, Templated, Type, Deprecation, Name, Profile, Title, HrefLang) -> 
    Link = hal:create_Link(Href, Templated, Type, Deprecation, Name, Profile, Title),
    Link#link{hreflang = HrefLang}.

%%------------------------------------------------------------------------------
%% Function: add_link/3
%% Purpose:  Add the link to the hal record using the name
%% Args:     Hal is a hal record.
%%           Name is a bitstring.
%%           Link is a link record.
%% Returns:  A hal record with the link attached.
%%------------------------------------------------------------------------------
add_link(Hal = #hal{}, Name, Link = #link{}) -> 
    case maps:is_key(Name, Hal#hal.links) of
        true ->
            if Name =:= ?SELF -> Hal#hal{links = maps:put(?SELF, Link, Hal#hal.links)}; 
               true ->
                   Links = maps:get(Name, Hal#hal.links),
                   Links2 = if erlang:is_list(Links) -> 
                          Links;
                      true ->
                          [Links]
                   end,
                   Hal#hal{links = maps:put(Name, [Link|Links2], Hal#hal.links)}  
            end;
        false -> 
            Hal#hal{links = maps:put(Name, Link, Hal#hal.links)}
    end.

%%------------------------------------------------------------------------------
%% Function: add_link/2 
%% Purpose:  Add a link to the supplied hal record using the link name value 
%% Args:     Hal is a hal record
%%           Link is a link record with a defined name value
%% Returns:  A hal record with the link attached.
%%------------------------------------------------------------------------------
add_link(Hal = #hal{}, Link = #link{name = Name}) when Link#link.name =/= undefined -> 
    hal:add_link(Hal, Name, Link). 

%% @todo handle adding a link when it doesnt have a name value

%%------------------------------------------------------------------------------
%% Function: add_links/2 
%% Purpose:  Add the supplied map of links to the supplied hal
%% Args:     Hal is a hal record
%%           Linkis is a map of links 
%% Returns:  A hal record with the link attached.
%%------------------------------------------------------------------------------
add_links(Hal = #hal{}, Links) when erlang:is_map(Links) ->
    Fun = fun(Key, Value, H) ->
        hal:add_link(H, Key, Value)
    end,
    maps:fold(Fun, Hal, Links).

%%------------------------------------------------------------------------------
%% Function: add_links/3 
%% Purpose:  Add the supplied list of links to the supplied hal
%% Args:     Hal is a hal record
%%           Name is the key to place the links under
%%           Linkis is a list of links 
%% Returns:  A hal record with the link attached.
%%------------------------------------------------------------------------------
add_links(Hal = #hal{}, Name, Links) when erlang:is_list(Links) ->
    Fun = fun(NewHal, H) ->
        hal:add_link(H, Name, NewHal)
    end,
    lists:foldl(Fun, Hal, Links).

%%------------------------------------------------------------------------------
%% Function: add_state/2 
%% Purpose:  Add the supplied state values to the hal 
%% Args:     Hal is a hal record
%%           State is a map of key value pairs
%% Returns:  A hal record with supplied state.
%%------------------------------------------------------------------------------
add_state(Hal = #hal{}, State) when erlang:is_map(State) -> 
    Hal#hal{state = maps:merge(Hal#hal.state, State)}.

%%------------------------------------------------------------------------------
%% Function: add_state/3 
%% Purpose:  Add the supplied state to the hal 
%% Args:     Hal is a hal record
%%           Key is the state name
%%           Value is the state value
%% Returns:  A hal record with supplied state.
%%------------------------------------------------------------------------------
add_state(Hal = #hal{}, Key, Value) -> 
    Hal#hal{state = maps:put(Key, Value, Hal#hal.state)}.

%%------------------------------------------------------------------------------
%% Function: add_embedded/3 
%% Purpose:  An a hal record as an embedded record into the suppied hal
%% Args:     Hal is a hal record
%%           Name is the resource type the embedded hal record represents
%%           NewHal is the hal record to embed or a list of records to embed.
%% Returns:  A hal record with embedded hal 
%%------------------------------------------------------------------------------
add_embedded(Hal = #hal{}, Name, NewHal) when erlang:is_record(NewHal, hal) ->
    case  maps:is_key(Name, Hal#hal.embedded) of
       true ->
           Embedded = maps:get(Name, Hal#hal.embedded),
           Hal#hal{embedded = maps:put(Name, [NewHal|Embedded], Hal#hal.embedded)}; 
       false ->
           Hal#hal{embedded = maps:put(Name, [NewHal], Hal#hal.embedded)}
    end;
add_embedded(Hal = #hal{}, Name, List) when erlang:is_list(List) -> 
    Fun = fun(NewHal, H) -> 
        hal:add_embedded(H, Name, NewHal)
    end,
    lists:foldl(Fun, Hal, List).

%%------------------------------------------------------------------------------
%% Function: create_curie/1 
%% Purpose:  Create a curie record using the supplied href.
%% Args:     Href is a bitstring.
%% Returns:  A curie record. 
%%------------------------------------------------------------------------------
create_curie(Href) -> 
    #curie{href=Href}.

%%------------------------------------------------------------------------------
%% Function: create_curie/2
%% Purpose:  Create a curie record using the supplied href and name. 
%% Args:     Href is a bitstring.
%%           Name is a bitstring.
%% Returns:  A curie record.
%%------------------------------------------------------------------------------
create_curie(Href, Name) -> 
    Curie = hal:create_curie(Href),
    Curie#curie{name = Name}.

%%------------------------------------------------------------------------------
%% Function: add_curie/2
%% Purpose:  Add a curie record to the supplied hal recor
%% Args:     Hal is a hal record.
%%           Curie is a curie record..
%% Returns:  The hal record.
%%------------------------------------------------------------------------------
add_curie(Hal = #hal{}, Curie) when erlang:is_record(Curie, curie) -> 
    Curies = Hal#hal.curies,
    Hal#hal{curies = [Curie|Curies]}.

%%------------------------------------------------------------------------------
%% Function: hal_to_map/1 
%% Purpose:  Convert the supplied hal to a map. 
%% Args:     Hal is a hal record
%% Returns:  A map representation of the supplied hal. 
%%------------------------------------------------------------------------------
hal_to_map(#hal{links = Links, embedded = Embedded, state = State, curies = Curies}) ->
    M = case maps:size(State) > 0 of
        true -> hal:state_to_map(State);
        false -> maps:new() 
    end, 
    M2 = case maps:size(Links) > 0 of
        true -> M#{?LINKS => hal:links_to_map(Links)};
        false -> M
    end,
    M3 = case maps:size(Embedded) > 0 of
        true -> M2#{?EMBEDDED => hal:embedded_to_map(Embedded)};
        false -> M2
    end,
    M4 = case erlang:length(Curies) > 0 of
        true ->
            CurrentLinks = maps:get(?LINKS, M3, maps:new()),
            WithCurieLinks = maps:put(?CURIES, hal:curies_to_map(Curies), CurrentLinks),
            M3#{?LINKS => WithCurieLinks}; 
        false -> M3
    end,
    M4.

%%------------------------------------------------------------------------------
%% Function: state_to_map/1 
%% Purpose:  Convert the supplied state to a map. 
%% Args:     State is a map
%% Returns:  A map representation of the supplied map with the embedded and links 
%%           keys/values removed 
%%------------------------------------------------------------------------------
state_to_map(State) when erlang:is_map(State) -> 
    maps:without([?EMBEDDED, ?LINKS], State).

%%------------------------------------------------------------------------------
%% Function: links_to_map/1 
%% Purpose:  Convert the supplied links to a map. 
%% Args:     Links is map
%% Returns:  A map representation of the supplied links
%%------------------------------------------------------------------------------
links_to_map(Links) when erlang:is_map(Links) ->
    Fun = fun(Key, Value, Acc) ->
        if erlang:is_list(Value) =:= true ->
              LinksMap = lists:map(fun(L) -> hal:link_to_map(L) end, Value),
              maps:put(Key, LinksMap, Acc); 
           true -> 
               LinkMap = hal:link_to_map(Value), 
               maps:put(Key, LinkMap, Acc)
        end
    end,
    maps:fold(Fun, #{}, Links).

%%------------------------------------------------------------------------------
%% Function: link_to_map/1 
%% Purpose:  Convert the supplied link to a map. 
%% Args:     Link is a link record
%% Returns:  A map representation of the supplied link
%%------------------------------------------------------------------------------
link_to_map(Link = #link{href = Href, templated = Templated, type = Type, deprecation = Deprecation, name = Name, profile = Profile, title = Title, hreflang = HrefLang}) when erlang:is_record(Link, link) ->
    Map = maps:new(),
    HrefMap = 
        if Href =/= undefined -> maps:put(?HREF, Href, Map);
           true -> Map
        end,
    TemplatedMap = 
        if erlang:is_boolean(Templated) andalso Templated =:= true -> maps:put(?TEMPLATED, Templated, HrefMap);
           true -> HrefMap
        end,
    TypeMap = 
        if Type =/= undefined -> maps:put(?TYPE, Type, TemplatedMap);
           true -> TemplatedMap
        end,
    DeprecationMap = 
        if Deprecation =/= undefined -> maps:put(?DEPRECATION, Deprecation, TypeMap);
           true -> TypeMap
        end,
    NameMap = 
        if Name =/= undefined -> maps:put(?NAME, Name, DeprecationMap);
           true -> DeprecationMap
        end,
    ProfileMap = 
        if Profile =/= undefined -> maps:put(?PROFILE, Profile, NameMap);
           true -> NameMap
        end,
    TitleMap = 
        if Title =/= undefined -> maps:put(?TITLE, Title, ProfileMap);
           true -> ProfileMap
        end,
    HrefLangMap = 
        if HrefLang =/= undefined -> maps:put(?HREFLANG, HrefLang, TitleMap);
           true -> TitleMap
        end,
    HrefLangMap.
 
%%------------------------------------------------------------------------------
%% Function: embedded_to_map/1 
%% Purpose:  Convert the supplied embedded hal records to a map. 
%% Args:     Embedded should be a map 
%% Returns:  A map representation of the supplied embedded map
%%------------------------------------------------------------------------------
embedded_to_map(Embedded) when erlang:is_map(Embedded) -> 
    Fun = fun(Key, Value, Acc) -> 
        maps:put(Key, [hal:hal_to_map(H) || H <- Value], Acc)
    end,
    EmbeddedMap = maps:fold(Fun, #{}, Embedded),
    EmbeddedMap.

%%------------------------------------------------------------------------------
%% Function: curie_to_map/1 
%% Args:     A curie record.
%% Purpose:  Convert the supplied curie to a map.
%% Returns:  A Map representation of the curie.
%%------------------------------------------------------------------------------
curie_to_map(Curie = #curie{href = Href, name = Name, templated = Templated}) when erlang:is_record(Curie, curie) -> 
    Map = maps:new(),
    HrefMap =
        if Href =/= undefined -> maps:put(?HREF, Href, Map);
           true -> Map
        end,
    TemplatedMap = 
        if erlang:is_boolean(Templated) andalso Templated =:= true 
                -> maps:put(?TEMPLATED, Templated, HrefMap);
           true -> HrefMap
        end,
    NameMap =
        if Name =/= undefined -> maps:put(?NAME, Name, TemplatedMap);
           true -> TemplatedMap
        end,
    NameMap.

%%------------------------------------------------------------------------------
%% Function: curies_to_map/1
%% Purpose:  Test the hal module
%% Args:     A list of curie records
%% Returns:  A list contining the curie records converted to maps.
%%------------------------------------------------------------------------------
curies_to_map(Curies) when erlang:is_list(Curies) -> 
    lists:map(fun(Curie) -> hal:curie_to_map(Curie) end, Curies).


%%------------------------------------------------------------------------------
%% Function: test_create_hal
%% Purpose:  Test that we can create an empty hal record
%%------------------------------------------------------------------------------
create_hal_test() -> 
    Hal = hal:create_hal(),
    ?assert(erlang:is_record(Hal, hal)).

%%------------------------------------------------------------------------------
%% Function: create_hal_with_link_test
%% Purpose: Test that we can create an hal record with a self link
%%------------------------------------------------------------------------------
create_hal_with_bistring_link_test() -> 
    Link = <<"/">>,
    Hal = hal:create_hal(Link),
    ?assert(erlang:is_record(Hal, hal)).

%%------------------------------------------------------------------------------
%% Function: create_hal_with_link_test
%% Purpose:  Test that we can create a hal record with a link record
%%------------------------------------------------------------------------------
create_hal_with_link_test() -> 
    Link  = hal:create_link(<<"/">>),
    Hal = hal:create_hal(Link),
    ?assert(erlang:is_record(Hal, hal)).

%%------------------------------------------------------------------------------
%% Function: create_hal_with_state
%% Purpose: Test that we can create a hal record with state values
%%------------------------------------------------------------------------------
create_hal_with_state_test() -> 
    Self  = hal:create_link(<<"/">>),
    State = #{name => <<"Test">>},
    Hal = hal:create_hal(Self, State),
    ?assert(erlang:is_record(Hal, hal)).

%%------------------------------------------------------------------------------
%% Function: create_link_with_bitstring_test
%% Purpose: Test that we can create a link record with a bitstring
%%------------------------------------------------------------------------------
create_link_with_bitstring_test() -> 
    Link = hal:create_link(<<"/">>),
    ?assert(erlang:is_record(Link, link)).

%%------------------------------------------------------------------------------
%% Function: create_link_with_link_test
%% Purpose: Test that we can create a link using a link
%%------------------------------------------------------------------------------
create_link_with_link_test() -> 
    Link = hal:create_link(<<"/">>),
    NewLink = hal:create_link(Link),
    ?assert(erlang:is_record(NewLink, link)).

%%------------------------------------------------------------------------------
%% Function: create_templated_link_test
%% Purpose: Test that we can create a templated link
%%------------------------------------------------------------------------------
create_templated_link_test() -> 
    TemplatedLink = hal:create_link(<<"/">>, true),
    ?assert(erlang:is_record(TemplatedLink, link)),
    ?assert(true =:= TemplatedLink#link.templated),
    Link = hal:create_link(<<"/">>, false), 
    ?assert(false =:= Link#link.templated).

%%------------------------------------------------------------------------------
%% Function: create_type_link_test
%% Purpose: Test that we can create a link record with a type
%%------------------------------------------------------------------------------
create_type_link_test() -> 
    TypeLink = hal:create_link(<<"/">>, false, <<"text/html">>),
    ?assert(erlang:is_record(TypeLink, link)).

%%------------------------------------------------------------------------------
%% Function: create_deprecation_link_test
%% Purpose: Test that we can create a link record with a deprecation
%%------------------------------------------------------------------------------
create_deprecation_link_test() -> 
    DeprecationLink = hal:create_link(<<"/">>, false, <<"text/html">>, <<"/somelink">>),
   ?assert(erlang:is_record(DeprecationLink, link)). 

%%------------------------------------------------------------------------------
%% Function: create_name_link_test
%% Purpose: Test that we can create a link record with a name
%%------------------------------------------------------------------------------
create_name_link_test() -> 
    NameLink = hal:create_link(<<"/">>, false, undefined, undefined, <<"name">>),
    ?assert(erlang:is_record(NameLink, link)).

%%------------------------------------------------------------------------------
%% Function: create_profile_link_test
%% Purpose:  Test that we can create a link with a profile
%%------------------------------------------------------------------------------
create_profile_link_test() -> 
    ProfileLink = hal:create_link(<<"/">>, false, undefined, undefined, undefined),
    ?assert(erlang:is_record(ProfileLink, link)).

%%------------------------------------------------------------------------------
%% Function: create_title_link_test
%% Purpose:  Test that we can create a link with a title
%%------------------------------------------------------------------------------
create_title_link_test() -> 
    TitleLink = hal:create_link(<<"/">>, false, undefined, undefind, undefined, <<"title">>),
    ?assert(erlang:is_record(TitleLink, link)).

%%------------------------------------------------------------------------------
%% Function: create_hreflang_link_test
%% Purpose:  Test that we can create a link with a hreflang
%%------------------------------------------------------------------------------
create_hrefLang_link_test() -> 
    HrefLangLink = hal:create_link(<<"/">>, false, undefined, undefined, undefined, undefined, <<"en">>),
    ?assert(erlang:is_record(HrefLangLink, link)).

%%------------------------------------------------------------------------------
%% Function: add_link_name_test
%% Purpose: test that we can add a link 
%%------------------------------------------------------------------------------
add_link_name_test() -> 
    Hal  = hal:create_hal(),
    Name = <<"name">>,
    Link = hal:create_link(<<"/link">>),
    HalWithLink = hal:add_link(Hal, Name, Link),
    ?assert(erlang:is_record(HalWithLink, hal)),
    ?assert(maps:size(HalWithLink#hal.links) =:= 1).

%%------------------------------------------------------------------------------
%% Function: add_link_test
%% Purpose: test that we can add a link 
%%------------------------------------------------------------------------------
add_link_test() -> 
    Hal = hal:create_hal(),
    Link  = hal:create_link(<<"/other-link">>, false, undefined, undefined, <<"name">>),
    HalWithLink = hal:add_link(Hal, Link),
    ?assert(maps:size(HalWithLink#hal.links) =:= 1).

%%------------------------------------------------------------------------------
%% Function: add_list_of_links_with_name_test
%% Purpose: test that we can add a list of links 
%%------------------------------------------------------------------------------
add_list_of_links_with_name_test() -> 
    Hal = hal:create_hal(),
    Links = [
        hal:create_link(<<"/home">>),        
        hal:create_link(<<"/away">>),        
        hal:create_link(<<"/other">>)        
    ],
    Name = <<"test">>,
    ?assert(erlang:is_list(Links)),
    ?assert(3 =:= length(Links)),
    HalWithLinks = hal:add_links(Hal, Name, Links),
    ?assert(maps:size(HalWithLinks#hal.links) =:= 1).

%%------------------------------------------------------------------------------
%% Function: test/1 
%% Purpose:  Test the hal module
%% Returns:  ok on succes
%%------------------------------------------------------------------------------
test() ->
    % https://tools.ietf.org/html/draft-kelly-json-hal-08#section-3

    Ex3 = hal:create_hal(<<"/orders/523">>),
    WareHouseLink = hal:create_link(<<"/warehouse/56">>),
    InvoiceLink = hal:create_link(<<"/invoice/873">>),
    Ex3Links = hal:add_links(Ex3, #{<<"warehouse">> => WareHouseLink, <<"invoice">> => InvoiceLink}),
    State = #{<<"currency">> => "USD", <<"status">> => <<"shipped">>, <<"total">> => 10.20},
    Ex3State = hal:add_state(Ex3Links, State),

    io:format("Ex3: ~p~n~n", [hal:hal_to_map(Ex3State)]),
    
    % https://tools.ietf.org/html/draft-kelly-json-hal-08#section-6
    Ex6 = hal:create_hal(<<"/orders">>),
    NextLink = hal:create_link(<<"/orders?page=2">>),
    FindLink = hal:create_link(<<"/orders/{id}">>, true),
    Ex6State = #{<<"currentlyProcessing">> => 14, <<"shippedToday">> => 20},
    Ex6WithLinks = hal:add_links(Ex6, #{<<"next">> => NextLink, <<"find">> => FindLink}),
    Ex6WithState = hal:add_state(Ex6WithLinks, Ex6State),

    io:format("Ex6: ~p~n~n", [hal:hal_to_map(Ex6WithState)]),
   
    % honorific example
    Honorifics = [
        #{ <<"name">> => <<"male">>,   <<"title">> => <<"Male">>},
        #{ <<"name">> => <<"female">>, <<"title">> => <<"Female">>},
        #{ <<"name">> => <<"other">>,  <<"title">> => <<"Other">>}
    ],
    Hs = lists:map(fun(X) -> H = hal:create_hal(), hal:add_state(H, X) end, Honorifics), 
    H = hal:create_hal(<<"/honorifics">>),
    H2 = hal:add_state(H, #{<<"total">> => 3, <<"page">> => 1, <<"number_of_pages">> => 1, <<"limit">> => 10}),
    H3 = hal:add_embedded(H2, <<"honorific">>, Hs),
    io:format("Honorofics: ~p~n~n", [hal:hal_to_map(H3)]),

    

    % create an empty hal
    %EmptyHal = hal:create_hal(),
    %io:format("Empty Hal: ~p~n ~p~n", [EmptyHal, hal:hal_to_map(EmptyHal)]),
    
    % create hal with a bitstring
    %BitStringHal = hal:create_hal(<<"/bitstring">>),
    %io:format("BitString Hal: ~p~n ~p~n", [BitStringHal, hal:hal_to_map(BitStringHal)]),

    % create hal with a link
    %LinkHalLink = hal:create_link(<<"/link">>),
    %LinkHal = hal:create_hal(LinkHalLink),
    %io:format("LinkHal: ~p~n ~p~n", [LinkHal, hal:hal_to_map(LinkHal)]),

    % create hal with state
    %StateHal = hal:create_hal(<<"/state">>, #{<<"name">> => <<"state">>}),
    %io:format("StateHal: ~p~n ~p~n", [StateHal, hal:hal_to_map(StateHal)]),  

    % create link 
    %BitStringLink = hal:create_link(<<"/bitstring">>),
    %io:format("BitStringLink:~n~p~n ~p~n~n", [BitStringLink, hal:link_to_map(BitStringLink)]),

    %LinkLink = hal:create_link(hal:create_link(<<"/link">>)),
    %io:format("LinkLink: ~n~p~n ~p~n~n", [LinkLink, hal:link_to_map(LinkLink)]),

    % add a link to a hal
    %AddLinkHal = hal:create_hal(),
    %AddLinkHal2 = hal:add_link(AddLinkHal, <<"self">>, hal:create_link(<<"/add-link">>)),
    %io:format("AddLinkHal: ~n~p~n ~p~n~n", [AddLinkHal2, hal:hal_to_map(AddLinkHal2)]), 
   
    % add a link to the hal using the link name prooperty
    %AddLinkName     = hal:create_link(<<"/name">>, false, undefined, undefined, <<"Name">>),
    %AddLinkNameHal  = hal:create_hal(),
    %AddLinkNameHal2 = hal:add_link(AddLinkNameHal, AddLinkName),
    %io:format("AddLinkNameHal: ~n~p~n ~p~n~n", [AddLinkNameHal2, hal:hal_to_map(AddLinkNameHal2)]),

    % add a map of links
    LinkMap = #{
        <<"one">>   => hal:create_link(<<"/one">>),
        <<"two">>   => hal:create_link(<<"/two">>),
        <<"three">> => hal:create_link(<<"/three">>)
    },
    AddLinksHal = hal:create_hal(),
    AddLinksHal2 = hal:add_links(AddLinksHal, LinkMap),
    %io:format("AddLinksHal: ~n~p~n ~p~n~n", [AddLinksHal2, hal:hal_to_map(AddLinksHal2)]),
    
    % add links under the same name
    LinkList = [ 
         hal:create_link(<<"sheep">>) | 
         [hal:create_link(<<"bar">>) | 
         [ hal:create_link(<<"gum">>) |
         [ hal:create_link(<<"cheese">>) |
         [ hal:create_link(<<"spam">>) |
         [ hal:create_link(<<"lettuce">>) 

]]]]]],
    
    %L = lists:map(fun(L) -> link_to_map(L) end, LinkList),
    %io:format("~p", [L]),

    %Ls = #{<<"pages">> => LinkList},
    %io:format("~p", [hal:links_to_map(Ls)])
    AddLinkListHal  = hal:create_hal(<<"/link-list-hal">>),
    AddLinkListHal2 = hal:add_links(AddLinkListHal, <<"link-list">>, LinkList),
    %io:format("AddLinkListHal: ~n~p~n ~n~n", [AddLinkListHal2]),
    %io:format("~p~n~n", [hal:hal_to_map(AddLinkListHal2)]),

    % add state using map
    StateHal = hal:create_hal(<<"/state">>),
    StateHalState = #{<<"name">> => <<"David">>, <<"age">> => 36},
    StateHal2 = hal:add_state(StateHal, StateHalState),
    % io:format("StateHal: ~n~p~n ~p~n~n", [StateHal2, hal:hal_to_map(StateHal2)]),
   
    StateHalUpdated = hal:add_state(StateHal2, <<"gender">>, <<"male">>), 
    %io:format("StateHalUpdated: ~n~p~n ~p~n~n", [StateHalUpdated, hal:hal_to_map(StateHalUpdated)]),

    EmbeddedHal = hal:create_hal(<<"/embedded">>),
    HalWithEmbedded = hal:add_embedded(StateHalUpdated, <<"embedded">>, EmbeddedHal),
    %io:format("HalWithEmbedded: ~n~p~n ~p~n~n", [HalWithEmbedded, hal:hal_to_map(HalWithEmbedded)]),

    Curie = hal:create_curie(<<"https://docs.acme.com/relations/{ref}">>, <<"acme">>),
    %io:format("Curie: ~n~p~n ~p~n~n", [Curie, hal:curie_to_map(Curie)]),

    CurieHal = hal:create_hal(),
    CurieHal2 = hal:add_curie(CurieHal, Curie),
    %io:format("Curied Hal: ~n~p~n", [hal:hal_to_map(CurieHal2)])
    ok
     .
