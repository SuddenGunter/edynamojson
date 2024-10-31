-module(edynamojson).

-export([
    serialize_document/1,
    deserialize_document/1
]).

serialize_document(Obj) when is_map(Obj) ->
    serialize(Obj);
serialize_document(_Obj) ->
    error(invalid_document_type).

deserialize_document(Obj) when is_map(Obj) ->
    deserialize(Obj);
deserialize_document(_Obj) ->
    error(invalid_document_type).

serialize(Obj) ->
    error(not_implemented).

deserialize(Obj) ->
    error(not_implemented).
