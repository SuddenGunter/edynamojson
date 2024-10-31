-module(edynamojson).

-moduledoc("Erlang DynamoDB JSON serializer/deserializer.").

-export([serialize_term/1, serialize_json/1, deserialize_term/1]).

%% @doc Serialize term into DynamoDB acceptable JSON. See README.md for usage examples.
%% @throws error(invalid_map_key_type | invalid_document_type | invalid_kv_tuple | unsupported_field_type | unknown_type_in_numbers_set)
-spec serialize_json(map()) -> binary().
serialize_json(Obj) when is_map(Obj) ->
    Term = serialize_term(Obj),
    iolist_to_binary(json:encode(Term)).

%% @doc Serialize term into DynamoDB acceptable format. This function
%% returns an Erlang term, not a JSON binary. See README.md for usage examples.
%% @throws error(invalid_map_key_type | invalid_document_type | invalid_kv_tuple | unsupported_field_type | unknown_type_in_numbers_set)
-spec serialize_term(map()) -> map().
serialize_term(Obj) when is_map(Obj) ->
    ValidKeys = valid_keys(maps:keys(Obj)),
    if ValidKeys ->
           maps:map(fun(_K, V) -> serialize(V) end, Obj);
       true ->
           error(invalid_map_key_type)
    end;
serialize_term(_Obj) ->
    error(invalid_document_type).

%% @doc Deserialize DynamoDB-style term into readable map. Basically, this is the
%% opposite of serialize_term.
-spec deserialize_term(map()) -> map().
deserialize_term(Obj) when is_map(Obj) ->
    deserialize(Obj);
deserialize_term(_Obj) ->
    error(invalid_document_type).

serialize(Obj) when is_map(Obj) ->
    ValidKeys = valid_keys(maps:keys(Obj)),
    SerializedEntries =
        if ValidKeys ->
               maps:map(fun(_K, V) -> serialize(V) end, Obj);
           true ->
               error(invalid_map_key_type)
        end,
    #{<<"M">> => SerializedEntries};
serialize(Obj) when is_list(Obj) ->
    #{<<"L">> => lists:map(fun(X) -> serialize(X) end, Obj)};
serialize(Obj) when is_binary(Obj) ->
    #{<<"S">> => Obj};
serialize(Obj) when is_number(Obj) ->
    #{<<"N">> => list_to_binary(integer_to_list(Obj))};
serialize(Obj) when is_boolean(Obj) ->
    #{<<"BOOL">> => Obj};
serialize({K, V}) when is_binary(K) ->
    ValidKV = valid_kv(K, V),
    if ValidKV ->
           #{K => V};
       true ->
           error(invalid_kv_tuple)
    end;
serialize(_Obj) ->
    error(unsupported_field_type).

deserialize(_Obj) ->
    error(not_implemented).

valid_keys([H | T]) when is_binary(H) ->
    valid_keys(T);
valid_keys([]) ->
    true;
valid_keys(_Keys) ->
    false.

valid_kv(<<"NULL">>, true) ->
    true;
valid_kv(<<"S">>, V) when is_binary(V) ->
    true;
valid_kv(<<"B">>, V) when is_binary(V) ->
    true;
valid_kv(<<"N">>, V) when is_number(V) ->
    true;
valid_kv(<<"BOOL">>, V) when is_boolean(V) ->
    true;
valid_kv(<<"M">>, V) when is_map(V) ->
    true;
valid_kv(<<"L">>, V) when is_list(V) ->
    true;
valid_kv(<<"SS">>, V) when is_list(V) ->
    lists:all(fun(X) -> is_binary(X) end, V);
valid_kv(<<"NS">>, V) when is_list(V) ->
    lists:map(fun(X) ->
                 if is_number(X) -> list_to_binary(integer_to_list(X));
                    is_binary(X) -> X;
                    true -> error(unknown_type_in_numbers_set)
                 end
              end,
              V);
valid_kv(<<"BS">>, V) when is_list(V) ->
    lists:all(fun(X) -> is_binary(X) end, V);
valid_kv(_K, _V) ->
    false.
