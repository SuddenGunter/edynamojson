-module(edynamojson).

-export([serialize_document/1, deserialize_document/1]).

serialize_document(Obj) when is_map(Obj) ->
    ValidKeys = valid_keys(maps:keys(Obj)),
    if ValidKeys ->
           maps:map(fun(_K, V) -> serialize(V) end, Obj);
       true ->
           error(invalid_map_key_type)
    end;
serialize_document(_Obj) ->
    error(invalid_document_type).

deserialize_document(Obj) when is_map(Obj) ->
    deserialize(Obj);
deserialize_document(_Obj) ->
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
% support custom-enforced types like {"NULL", true} or {"B", <<"base64 encoded value">>} etc
serialize({K, V}) when is_binary(K) ->
    ValidKV = valid_kv(K, V),
    if ValidKV ->
           #{K => V};
       true ->
           error(invalid_kv_tuple)
    end;
serialize(_Obj) ->
    error(unsupported_field_type).

deserialize(Obj) ->
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
