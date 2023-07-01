# Rate limit

This template shares many utilities to create a somewhat complex example around
rate limit rules.

## Instantiate

Create a law-stone instance with your version of the `gov.pl` file, and get its
smart contract.

## Usage

Using the example smart contract:
```console
$ export SMART_CONTRACT=okp41rkez4gh0s4jxcscu4e8alnnc8jeksztnfjstjkqjmntatkrq4gpqya4r6q
$ alias query="okp4d query wasm contract-state smart $SMART_CONTRACT \
    --chain-id okp4-nemeton-1 \
    --node https://api.testnet.okp4.network:443/rpc"
```


You can check if you are still allowed to send a request depending on your
requests or data limits.
```console
$ query "{\"ask\": {\"query\": \"can_query('User1').\"}}" | yq '.data.answer.success'
true
```

You can get your current quota:
```console
$ query "{\"ask\": {\"query\": \"get_usage('User1', Result).\"}}" | yq '.data.answer.results[0].substitutions[0].term.name'
"'984 requests / 1000; 510656512 bytes / 524288000'"
```

And a lot more (see developer utils below):
```console
query "{\"ask\": {\"query\": \"convert_bytes(510656512, Unit, Result).\"}}" | yq '.data.answer.results[0].substitutions[] | {(.variable): .term.name}'
{
  "Unit": "'MB'"
}
{
  "Result": "487.0"
}
```


## Developer utils

Some predicates that can be useful in various programs. For example:

**upcase_atom**: convert lowercase strings to uppercase
```console
?- upcase_atom('50Mb', X).
'50MB'
```

**atom_number**: convert strings to numbers and vice-versa

```console
?- atom_number('51', X).
51
?- atom_number(X, 51).
'51'
```

**timeframe_unit_above**: get the closest atom unit containing your timeframe
```console
?- timeframe_unit_above(3000, X).
hour
```

**ceil & round**: basic math stuff unsupported at this time
```console
?- ceil(3.3, X).
4
?- round(3.3, X).
3
```

**random_between**: ugly random generator until it gets supported
```console
?- random_between(0, 10, X).
5
```

**random_data_count**: generate data usage for testing purposes, until we can
query from cognitarium.
```console
?- random_data_count('Role1', X, 1000).
4MB
```

**data_unit_above**: same as timeframe_unit_above, but for data:
```console
?- data_unit_above(990, X).
'KB'
```

**split_data_unit**: separate count from unit, and case insensitive
```console
?- split_data_unit('500Mb', X, Y).
X = 500
Y = 'MB'
```
