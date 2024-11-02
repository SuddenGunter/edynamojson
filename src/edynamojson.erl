-module(edynamojson).

-moduledoc("Erlang DynamoDB JSON serializer/deserializer.").

-export([serialize_term/1, serialize_json/1, deserialize_term/1, deserialize_json/1]).

%% @doc Serialize term into DynamoDB acceptable JSON. See README.md for usage examples.
%% @throws error(invalid_map_key_type | invalid_document_type | unsupported_field_type )
-spec serialize_json(map()) -> binary().
serialize_json(Term) when is_map(Term) ->
    T = serialize_term(Term),
    iolist_to_binary(json:encode(T)).

%% @doc Serialize term into DynamoDB acceptable format. This function
%% returns an Erlang term, not a JSON binary. See README.md for usage examples.
%% @throws error(invalid_map_key_type | invalid_document_type | unsupported_field_type )
-spec serialize_term(map()) -> map().
serialize_term(Term) when is_map(Term) ->
    ValidKeys = valid_keys(maps:keys(Term)),
    if ValidKeys ->
           maps:map(fun(_K, V) -> serialize(V) end, Term);
       true ->
           error(invalid_map_key_type)
    end;
serialize_term(_Term) ->
    error(invalid_document_type).

-spec deserialize_json(binary()) -> map().
deserialize_json(Term) ->
    ParsedTerm = json:decode(Term),
    deserialize_term(ParsedTerm).

%% @doc Deserialize DynamoDB-style term into readable map. Basically, this is the
%% opposite of serialize_term.  This function returns an Erlang term, not a JSON binary.
%% See README.md for usage examples.
%% @throws error(invalid_document_type | invalid_map_key_type | invalid_kv_tuple | unsupported_field_type )
-spec deserialize_term(map()) -> map().
deserialize_term(Term) when is_map(Term) ->
    deserialize_layer(Term);
deserialize_term(_Term) ->
    error(invalid_document_type).

serialize(Term) when is_map(Term) ->
    ValidKeys = valid_keys(maps:keys(Term)),
    SerializedEntries =
        if ValidKeys ->
               maps:map(fun(_K, V) -> serialize(V) end, Term);
           true ->
               error(invalid_map_key_type)
        end,
    #{<<"M">> => SerializedEntries};
serialize(Term) when is_list(Term) ->
    #{<<"L">> => lists:map(fun(X) -> serialize(X) end, Term)};
serialize(Term) when is_binary(Term) ->
    #{<<"S">> => Term};
serialize(Term) when is_number(Term) ->
    #{<<"N">> => integer_to_binary(Term)};
serialize(Term) when is_boolean(Term) ->
    #{<<"BOOL">> => Term};
serialize(Term) when is_tuple(Term) ->
    serialize(#{<<"__tuple__">> => tuple_to_list(Term)});
serialize(null) ->
    #{<<"NULL">> => true};
serialize(_Term) ->
    error(unsupported_field_type).

valid_keys([H | T]) when is_binary(H) ->
    valid_keys(T);
valid_keys([]) ->
    true;
valid_keys(_Keys) ->
    false.

deserialize_layer(#{<<"__tuple__">> := #{<<"L">> := List} = ListField})
    when is_list(List) ->
    list_to_tuple(deserialize_field(ListField));
deserialize_layer(Term) when is_map(Term) ->
    ValidKeys = valid_keys(maps:keys(Term)),
    if ValidKeys ->
           maps:map(fun(_K, V) -> deserialize_field(V) end, Term);
       true ->
           error(invalid_map_key_type)
    end;
deserialize_layer(_Term) ->
    error(invalid_document_type).

deserialize_field(#{<<"M">> := Map}) ->
    deserialize_layer(Map);
deserialize_field(#{<<"S">> := Str}) when is_binary(Str) ->
    Str;
deserialize_field(#{<<"N">> := Num}) when is_binary(Num) ->
    binary_to_integer(Num);
deserialize_field(#{<<"BOOL">> := Bool}) when is_boolean(Bool) ->
    Bool;
deserialize_field(#{<<"L">> := List}) when is_list(List) ->
    lists:map(fun(X) -> deserialize_field(X) end, List);
deserialize_field(#{<<"B">> := Str}) when is_binary(Str) ->
    Str;
deserialize_field(#{<<"SS">> := List}) when is_list(List) ->
    AllBins = lists:all(fun(X) -> is_binary(X) end, List),
    if AllBins ->
           List;
       true ->
           error(invalid_kv_tuple)
    end;
deserialize_field(#{<<"BS">> := List}) when is_list(List) ->
    AllBins = lists:all(fun(X) -> is_binary(X) end, List),
    if AllBins ->
           List;
       true ->
           error(invalid_kv_tuple)
    end;
deserialize_field(#{<<"NS">> := List}) when is_list(List) ->
    AllBins = lists:all(fun(X) -> is_binary(X) end, List),
    if AllBins ->
           lists:map(fun(X) -> binary_to_integer(X) end, List);
       true ->
           error(invalid_kv_tuple)
    end;
deserialize_field(#{<<"NULL">> := true}) ->
    null;
deserialize_field(_V) ->
    error(unsupported_field_type).
