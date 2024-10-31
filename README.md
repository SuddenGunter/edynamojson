# edynamojson

Erlang DynamoDB JSON serializer/deserializer.


This library is primarily designed for use with [aws-beam](https://github.com/aws-beam/aws-erlang/tree/master/src). Since aws-beam automatically handles JSON serialization and deserialization, this library omits those steps and only converts your object into a format compatible with DynamoDB.

If you use another AWS client or make HTTP calls directly you need to also add JSON serialization step (see examples).

## Examples 

Serialization and write to DynamoDB with aws-beam:

```erl
Model = #{<<"id">> => <<"secret_id">>,
          <<"custom_type">> => {<<"NULL">>, true},
          <<"created_at">> => 123,
          <<"embedded_map">> => #{<<"foo">> => <<"bar">>}},

SerializedModel = edynamojson:serialize(Model),

Client = aws_client:make_client(AWSKey, AWSSecKey, AWSReg),

Data =  aws_dynamodb:put_item(Client, #{<<"TableName">> => <<"testdb">>,<<"Item">> => SerializedModel}),

```

Serialization and write to DynamoDB with aws-beam:

```erl
Model = #{<<"id">> => <<"secret_id">>,
          <<"custom_type">> => {<<"NULL">>, true},
          <<"created_at">> => 123,
          <<"embedded_map">> => #{<<"foo">> => <<"bar">>}},

SerializedModel = edynamojson:serialize(Model),

JSONModel = iolist_to_binary(json:encode(SerializedModel)),

my_dynamodb_client:put_item(JSONModel),

```

## FAQ

1. Why serialize returns / deserialize accepts only Erlang term instead of JSON?

- main use-case for this library is to be a convinience utility for aws-beam, which already does JSON serialization/deserialization
- you might want to use some specific json library so I don't want this package to have another dependency

2. Which Erlang types are supported?

| Erlang type | Support |
| -------- | ------- |
| Binaries | OK     |
| Numbers    | OK    |
| Lists    | OK    |
| Maps    | OK    |
| Boolean    | OK    |
| Atoms  | Unsupported* |
| Records    | Unsupported** |
| Tuples | With restrictions (see below) |
| Everything else | Untested |

*Atoms are currently unsupported, but I want to have an ability to pass option which could automatially serialize them as strings. No plans to allow deserializing DynamoDB items keys/values into atoms.

**Records are unsupported, I've no plan to support them. If you need it - create an issue or PR.

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

You can use 2 element tuples to pass `{ field_type, field_value }` pair. Library will try to do basic validation and allow you field to pass through without change. 

When to use this feature?

- when you want to use base64 encoded binary fields (library can't distinguish them from strings)
- when you want to use sets (SS, NS, BS). For number sets (NS) numbers can be passed both as binaries or numbers `[<<"1">>, 2]` - they will be converted to binaries when sending to DynamoDB anyway.
- when you want to use NULL field. Boolean true is the only acceptable value for this field.

```erl 

#{
    <<"NullField">> => {<<"NULL">>, true},
    <<"StringSetField">> => {<<"SS">>, [<<"foo">>, <<"bar">>]},
    <<"BinaryField">> => { <<"B">>, <<"Zm9vCg==">>}
}

```

## TODO:

- tests
- deserialization
