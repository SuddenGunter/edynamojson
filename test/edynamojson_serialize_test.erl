-module(edynamojson_serialize_test).

-include_lib("eunit/include/eunit.hrl").

-record(dynamo_msg, {id, value}).

unsupported_type_test_() ->
    [?_test(unsupported_type_base(fun(X) -> X end)),
     ?_test(unsupported_type_base(self())),
     ?_test(unsupported_type_base(atom)),
     ?_test(unsupported_type_base(make_ref())),
     ?_test(unsupported_type_base({1, 2, 3})),
     ?_test(unsupported_type_base(#dynamo_msg{id = 123, value = 321}))].

unsupported_type_base(Field) ->
    Input = #{<<"test_field">> => Field},
    ?assertException(error, unsupported_field_type, edynamojson:serialize_document(Input)).

invalid_kv_tuple_test() ->
    Input = #{<<"test_field">> => {<<"FAKE_TYPE">>, <<"V">>}},
    ?assertException(error, invalid_kv_tuple, edynamojson:serialize_document(Input)).

invalid_document_type_test_() ->
    [?_test(invalid_document_type_base(fun(X) -> X end)),
     ?_test(invalid_document_type_base(self())),
     ?_test(invalid_document_type_base(atom)),
     ?_test(invalid_document_type_base(make_ref())),
     ?_test(invalid_document_type_base({1, 2, 3})),
     ?_test(invalid_document_type_base(#dynamo_msg{id = 123, value = 321})),
     ?_test(invalid_document_type_base(<<"str">>)),
     ?_test(invalid_map_key_type_base("str")),
     ?_test(invalid_document_type_base(123)),
     ?_test(invalid_document_type_base([123])),
     ?_test(invalid_document_type_base(true))].

invalid_document_type_base(Input) ->
    ?assertException(error, invalid_document_type, edynamojson:serialize_document(Input)).

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
    ?assertException(error, invalid_map_key_type, edynamojson:serialize_document(Input)).
