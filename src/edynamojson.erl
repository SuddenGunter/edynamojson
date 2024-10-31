-module(edynamojson).

-export([serialize_document/1, deserialize_document/1]).

serialize_document(Obj) when is_map(Obj) ->
    serialize(Obj);
serialize_document(_Obj) ->
    error(invalid_document_type).

deserialize_document(Obj) when is_map(Obj) ->
    deserialize(Obj);
deserialize_document(_Obj) ->
    error(invalid_document_type).

serialize(Obj) when is_map(Obj) ->
    ValidKeys = valid_keys(maps:keys(Obj)),
    if ValidKeys ->
           maps:map(fun(_K, V) -> serialize(V) end, Obj);
       true ->
           error(invalid_map_key_type)
    end;
serialize(Obj) when is_list(Obj) ->
    error(not_implemented);
serialize(Obj) when is_binary(Obj) ->
    #{<<"S">> => Obj};
serialize(Obj) when is_number(Obj) ->
    #{<<"N">> =>
          Obj}; % todo: test if driver accepts this, or if I need to wrap it into a string
serialize(Obj) when is_boolean(Obj) ->
    #{<<"BOOL">> =>
          Obj}; % todo: test if driver accepts this, or if I need to wrap it into a string
serialize(Obj) when is_atom(Obj) ->
    error(not_implemented);  % todo: if allowed - conv to binary string, if not error
serialize(Obj) when is_tuple(Obj) ->
    error(not_implemented); % support custom-enforced types like {"NULL", true} or {"B", <<"base64 encoded value">>} etc
serialize(Obj) ->
    error(unsupported_field_type).

deserialize(Obj) ->
    error(not_implemented).

valid_keys([H | T]) when is_binary(H) ->
    valid_keys(T);
valid_keys([]) ->
    true;
valid_keys(_Keys) ->
    false.
