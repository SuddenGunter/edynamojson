# edynamojson

Erlang DynamoDB JSON serializer/deserializer.


This library is primarily designed for use with [aws-beam](https://github.com/aws-beam/aws-erlang/tree/master/src). Since aws-beam automatically handles JSON serialization and deserialization, this library omits those steps and only converts your object into a format compatible with DynamoDB.

If you use another AWS client or make HTTP calls directly, you also need to add the JSON serialization step (see examples).

## What?

DynamoDB does not allow you to just store objects like `{"id": "ABC"}`; it will always ask you to add type annotations like `{"id": {"S": "ABC"}}`.

This library does this automatically - so you don't have to.

## Examples 

Serialization and write to DynamoDB with aws-beam:

```erl
Model = #{<<"id">> => <<"secret_id">>,
          <<"created_at">> => 123,
          <<"embedded_map">> => #{<<"foo">> => <<"bar">>}},

SerializedModel = edynamojson:serialize(Model),

Client = aws_client:make_client(AWSKey, AWSSecKey, AWSReg),

Data =  aws_dynamodb:put_item(Client, #{<<"TableName">> => <<"testdb">>,<<"Item">> => SerializedModel}),

```

Serialization and write to DynamoDB with custom AWS client:

```erl
Model = #{<<"id">> => <<"secret_id">>,
          <<"created_at">> => 123,
          <<"embedded_map">> => #{<<"foo">> => <<"bar">>}},

SerializedModel = edynamojson:serialize(Model),

JSONModel = iolist_to_binary(json:encode(SerializedModel)),

my_dynamodb_client:put_item(JSONModel),

```

Read and deserialization: TBD (not implemented)

## FAQ

1. Why does serialize returns / deserialize accept only Erlang term instead of JSON?

- the main use case for this library is to be a convenience utility for aws-beam, which already does JSON serialization/deserialization
- you might want to use some specific JSON library, so I don't want this package to have another dependency

2. Which Erlang types are supported?

| Erlang type | Support |
| -------- | ------- |
| Binary | OK     |
| Number    | OK    |
| List   | OK    |
| Map    | OK    |
| Boolean    | OK    |
| String | Treated as Lists of numbers |
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

- tests
- deserialization
