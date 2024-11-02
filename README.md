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

Deserialization of data returned by aws-beam:

```erl
Client = aws_client:make_client(AWSKey, AWSSecKey, AWSReg),

Data =  aws_dynamodb:get_item(Client, #{<<"TableName">> => <<"testdb">>,<<"Key">> => Key}),

Term = edynamojson:deserialize_term(Data),

```

Deserialization of raw dynamoDB JSON:

```erl

% returns #{<<"A">> => <<"B">>}
Result = edynamojson:deserialize_json(<<"{\"A\":{\"S\":\"B\"}}">>),

```

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
| Tuple | OK (as map+list) |
| Function | - |
| Atom  | OK (as map+binary) |
| Record | OK (as tuple and atom) |
| PID | - |
| Reference | - |
| Everything else | Untested |

- Undefined behaviour will happen if you try to use untested types in production. If you really want to do it - test it meticulously.
- Tuple `{1, 2, 3}` is serialized as `#{<<"M">> => #{<<"__tuple__">> => #{ <<"L">> => [...dynamodb encoded numbers...]}}}`
- *Atoms are only allowed as field values*, never as maps keys. Atom `abc` will be serialized as `#{<<"M">> => #{<<"__atom__">> => #{<<"S">> => <<"abc>>}}}`. `null` atom is a special case that allows us to support DynamoDB's NULL type, so it will be serialized as `#{<<"NULL">> => true}`.

3. Which DynamoDB types are supported?

| DynamoDB type | Serialize | Deserialize |
| -------- | ------- |
| S | OK | OK |
| N | OK | OK |
| M | OK | OK |
| L | OK | OK |
| BOOL | OK | OK |
| NULL | OK* | OK* |
| NS, SS, BS | - | OK |
| B | - | OK |

*to represent dynamoDB's NULL in erlang we use `null` atom, like say `#{<<"A">> => null}` is serialized into `#{<<"A">> => #{<<"NULL">> => true}}`

## TODO:

- Elixir/Gleam examples
- benchmarks + optimizations (potentially can remove some validations to improve performance)
