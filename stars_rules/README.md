# Cosmos Stargaze address rules

This utility helper helps verify rules on any cosmos address, using a whitelist
of stargase addresses.

## Instantiate

Create a law-stone instance with your version of the `gov.pl` file, and get its
smart contract.

## Usage

Using the example smart contract:
```console
$ export SMART_CONTRACT=okp41swn7242ct59gzkypetuas3sd499sjr9ky4jw85w22qhnngjya5tsweee2y
$ alias query_stars_rules="okp4d query wasm contract-state smart $SMART_CONTRACT \
    --chain-id okp4-nemeton-1 \
    --node https://api.testnet.okp4.network:443/rpc"
```

You can verify that a DID address can access a database resource:
```console
$ query_stars_rules "{\"ask\": {\"query\": \"can('access_resource', 'did:key:cosmos1wy8ywt98sv5pmsg873rct4pxtw0ntxuwt3w29g', database1).\"}}" | yq '.data.answer.success'

true
```

You can convert any address to any chain format:
```console
$ query_stars_rules "{\"ask\": {\"query\": \"convert_address_to('cosmos1wy8ywt98sv5pmsg873rct4pxtw0ntxuwt3w29g', okp4, X).\"}}" | yq '.data.answer.results[0].substitutions[0].term.name'

"okp41wy8ywt98sv5pmsg873rct4pxtw0ntxuwxdtzen"
```
