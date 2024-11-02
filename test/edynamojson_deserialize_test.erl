-module(edynamojson_deserialize_test).

-include_lib("eunit/include/eunit.hrl").

-record(dynamo_msg, {id, value}).

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
    ?assertException(error, invalid_document_type, edynamojson:deserialize_term(Input)).

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
    ?assertException(error, invalid_map_key_type, edynamojson:deserialize_term(Input)).

deserialize_term_test_() ->
    [?_test(deserialize_term_base(#{}, #{})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"S">> => <<"B">>}}, #{<<"A">> => <<"B">>})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"N">> => <<"123">>}}, #{<<"A">> => 123})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"BOOL">> => true}}, #{<<"A">> => true})),
     ?_test(deserialize_term_base(#{<<"A">> =>
                                        #{<<"M">> => #{<<"foo">> => #{<<"S">> => <<"bar">>}}}},
                                  #{<<"A">> => #{<<"foo">> => <<"bar">>}})),
     ?_test(deserialize_term_base(#{<<"A">> =>
                                        #{<<"L">> =>
                                              [#{<<"N">> => <<"1">>}, #{<<"N">> => <<"2">>}]}},
                                  #{<<"A">> => [1, 2]})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"L">> => [#{<<"N">> => <<"66">>}]}},
                                  #{<<"A">> => "B"})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"B">> => <<"Zm9vCg==">>}},
                                  #{<<"A">> => <<"Zm9vCg==">>})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"SS">> => [<<"foo">>]}},
                                  #{<<"A">> => [<<"foo">>]})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"NS">> => [<<"123">>]}},
                                  #{<<"A">> => [123]})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"NS">> => [<<"123">>]}},
                                  #{<<"A">> => [123]})),
     ?_test(deserialize_term_base(#{<<"A">> => #{<<"NULL">> => true}}, #{<<"A">> => null})),
     ?_test(deserialize_term_base(#{<<"A">> =>
                                        #{<<"M">> =>
                                              #{<<"__atom__">> => #{<<"S">> => <<"atom">>}}}},
                                  #{<<"A">> => atom})),
     ?_test(deserialize_term_base(#{<<"A">> =>
                                        #{<<"M">> =>
                                              #{<<"__tuple__">> =>
                                                    #{<<"L">> =>
                                                          [#{<<"S">> => <<"A">>},
                                                           #{<<"S">> => <<"foo">>}]}}}},
                                  #{<<"A">> => {<<"A">>, <<"foo">>}})),
     ?_test(deserialize_term_base(#{<<"A">> =>
                                        #{<<"M">> =>
                                              #{<<"__tuple__">> =>
                                                    #{<<"L">> =>
                                                          [#{<<"M">> =>
                                                                 #{<<"__atom__">> =>
                                                                       #{<<"S">> =>
                                                                             <<"dynamo_msg">>}}},
                                                           #{<<"N">> => <<"123">>},
                                                           #{<<"N">> => <<"321">>}]}}}},
                                  #{<<"A">> => #dynamo_msg{id = 123, value = 321}}))].

deserialize_term_base(Input, Output) ->
    ?assertEqual(Output, edynamojson:deserialize_term(Input)).

deserialize_json_test_() ->
    [?_test(deserialize_json_base(<<"{}">>, #{})),
     ?_test(deserialize_json_base(<<"{\"A\":{\"S\":\"B\"}}">>, #{<<"A">> => <<"B">>})),
     ?_test(deserialize_json_base(<<"{\"A\":{\"N\":\"123\"}}">>, #{<<"A">> => 123}))].

deserialize_json_base(Input, Output) ->
    ?assertEqual(Output, edynamojson:deserialize_json(Input)).
