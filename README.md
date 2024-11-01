# edynamojson

Erlang DynamoDB JSON serializer/deserializer.


This library is primarily designed for use with [aws-beam](https://github.com/aws-beam/aws-erlang/tree/master/src). Since aws-beam automatically handles JSON serialization and deserialization, this library allows to just convert Erlang term to DynamoDB-styled Erlang term (and back).

Use `serialize_term/1` and `deserialize_term/1` for aws-beam or custom JSON serializers. Use `serialize_json/1` and `deserialize_json/1` for everything else.

## What?

DynamoDB does not allow you to just store objects like `{"id": "ABC"}`; it will always ask you to add type annotations like `{"id": {"S": "ABC"}}`.

This library does this automatically - so you don't have to.

## Examples 

Serialization and write to DynamoDB with aws-beam:

```erl
Model = #{<<"id">> => <<"secret_id">>,
          <<"created_at">> => 123,
          <<"embedded_map">> => #{<<"foo">> => <<"bar">>}},

% returns term: #{<<"id">>: {<<"S">>: <<"secret_id">>}..}>>
SerializedModel = edynamojson:serialize_term(Model),

Client = aws_client:make_client(AWSKey, AWSSecKey, AWSReg),

Data =  aws_dynamodb:put_item(Client, #{<<"TableName">> => <<"testdb">>,<<"Item">> => SerializedModel}),

```

Serialization and write to DynamoDB with custom AWS client:

```erl
Model = #{<<"id">> => <<"secret_id">>,
          <<"created_at">> => 123,
          <<"embedded_map">> => #{<<"foo">> => <<"bar">>}},

% returns JSON as binary: <<"{\"id\": { \"S\": \"secret_id\"}..}>>
SerializedModel = edynamojson:serialize_json(Model),

my_dynamodb_client:put_item(SerializedModel),

```

Read and deserialization: TBD (not implemented)

## FAQ

1. Why provide `serialize_term/1` / `deserialize_term/1` ?

- the main use case for this library is to be a convenience utility for aws-beam, which already does JSON serialization/deserialization
- you might want to use some specific JSON library, so I don't want this package to have another dependency that you don't need

2. Which Erlang types are supported?

| Erlang type | Support |
| -------- | ------- |
| Binary | OK     |
| Number    | OK    |
| List   | OK    |
| Map    | OK    |
| Boolean    | OK    |
| String | Treated as a Lists of numbers |
| Tuple | Only for custom fields |
| Function | - |
| Atom  | - |
| PID | - |
| Reference | - |
| Everything else | Untested |

Undefined behaviour will happen if you try to use untested types in production. If you really want to do it - test it meticulously.

3. Which DynamoDB types are supported?

| DynamoDB type | Support |
| -------- | ------- |
| S | OK     |
| N    | OK    |
| M    | OK    |
| L    | OK    |
| BOOL    | OK    |
| NULL  | Via tuple |
| NS, SS, BS    | Via tuple |
| B    | Via tuple |

## Using tuples for custom fields

You can use two element tuples to pass `{ field_type, field_value }` pair. The library will try to do basic validation and allow your field to pass through without change. 

When to use this feature?

- when you want to use base64 encoded binary fields (the library can't distinguish them from regular binary strings)
- when you want to use sets (SS, NS, BS). For number sets (NS), numbers can be passed both as binaries or numbers `[<<"1">>, 2]` - they will be converted to binaries when sent to DynamoDB anyway.
- when you want to use the NULL field. Boolean true is the only acceptable value for this field.

```erl 

#{
    <<"NullField">> => {<<"NULL">>, true},
    <<"StringSetField">> => {<<"SS">>, [<<"foo">>, <<"bar">>]},
    <<"BinaryField">> => { <<"B">>, <<"Zm9vCg==">>}
}

```

## TODO:

- deserialization tests
- CI that runs tests + linters
- Elixir examples
- Gleam examples?
- benchmarks + optimizations (potentially can remove some validations to improve performance)
- forbid binaries in number sets