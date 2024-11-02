-module(edynamojson_serialize_test).

-include_lib("eunit/include/eunit.hrl").

-record(dynamo_msg, {id, value}).

unsupported_field_type_test_() ->
    [?_test(unsupported_field_type_base(fun(X) -> X end)),
     ?_test(unsupported_field_type_base(self())),
     ?_test(unsupported_field_type_base(atom)),
     ?_test(unsupported_field_type_base(make_ref())),
     ?_test(unsupported_field_type_base(#dynamo_msg{id = 123, value = 321}))].

unsupported_field_type_base(Field) ->
    Input = #{<<"test_field">> => Field},
    ?assertException(error, unsupported_field_type, edynamojson:serialize_term(Input)).

invalid_document_type_test_() ->
    [?_test(invalid_document_type_base(fun(X) -> X end)),
     ?_test(invalid_document_type_base(self())),
     ?_test(invalid_document_type_base(atom)),
     ?_test(invalid_document_type_base(make_ref())),
     ?_test(invalid_document_type_base({1, 2, 3})),
     ?_test(invalid_document_type_base(#dynamo_msg{id = 123, value = 321})),
     ?_test(invalid_document_type_base(<<"str">>)),
     ?_test(invalid_document_type_base("str")),
     ?_test(invalid_document_type_base(123)),
     ?_test(invalid_document_type_base([123])),
     ?_test(invalid_document_type_base(true))].

invalid_document_type_base(Input) ->
    ?assertException(error, invalid_document_type, edynamojson:serialize_term(Input)).

invalid_map_key_type_test_() ->
    [?_test(invalid_map_key_type_base(fun(X) -> X end)),
     ?_test(invalid_map_key_type_base(self())),
     ?_test(invalid_map_key_type_base(atom)),
     ?_test(invalid_map_key_type_base(make_ref())),
     ?_test(invalid_map_key_type_base({1, 2, 3})),
     ?_test(invalid_map_key_type_base(#dynamo_msg{id = 123, value = 321})),
     ?_test(invalid_map_key_type_base("str")),
     ?_test(invalid_map_key_type_base(123)),
     ?_test(invalid_map_key_type_base([123])),
     ?_test(invalid_map_key_type_base(true))].

invalid_map_key_type_base(Key) ->
    Input = #{Key => <<"test_field">>},
    ?assertException(error, invalid_map_key_type, edynamojson:serialize_term(Input)).

serialize_term_test_() ->
    [?_test(serialize_term_base(#{}, #{})),
     ?_test(serialize_term_base(#{<<"A">> => <<"B">>}, #{<<"A">> => #{<<"S">> => <<"B">>}})),
     ?_test(serialize_term_base(#{<<"A">> => 123}, #{<<"A">> => #{<<"N">> => <<"123">>}})),
     ?_test(serialize_term_base(#{<<"A">> => true}, #{<<"A">> => #{<<"BOOL">> => true}})),
     ?_test(serialize_term_base(#{<<"A">> => #{<<"foo">> => <<"bar">>}},
                                #{<<"A">> =>
                                      #{<<"M">> => #{<<"foo">> => #{<<"S">> => <<"bar">>}}}})),
     ?_test(serialize_term_base(#{<<"A">> => [1, 2]},
                                #{<<"A">> =>
                                      #{<<"L">> =>
                                            [#{<<"N">> => <<"1">>}, #{<<"N">> => <<"2">>}]}})),
     ?_test(serialize_term_base(#{<<"A">> => "B"},
                                #{<<"A">> => #{<<"L">> => [#{<<"N">> => <<"66">>}]}})),
     ?_test(serialize_term_base(#{<<"A">> => {<<"A">>, <<"foo">>}},
                                #{<<"A">> =>
                                      #{<<"M">> =>
                                            #{<<"__tuple__">> =>
                                                  #{<<"L">> =>
                                                        [#{<<"S">> => <<"A">>},
                                                         #{<<"S">> => <<"foo">>}]}}}}))].

serialize_term_base(Input, Output) ->
    ?assertEqual(Output, edynamojson:serialize_term(Input)).

serialize_json_test_() ->
    [?_test(serialize_json_base(#{}, <<"{}">>)),
     ?_test(serialize_json_base(#{<<"A">> => <<"B">>}, <<"{\"A\":{\"S\":\"B\"}}">>)),
     ?_test(serialize_json_base(#{<<"A">> => 123}, <<"{\"A\":{\"N\":\"123\"}}">>))].

serialize_json_base(Input, Output) ->
    ?assertEqual(Output, edynamojson:serialize_json(Input)).
