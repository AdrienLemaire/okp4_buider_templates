# Cosmos chains access control

This utility helper allows one to propose rules according to addresses from any
Cosmos chains.

## Instantiate

Modify the `gov.pl` file according to your needs, create a new law-stone
instance and find its smart contract address:
```console
$ okp4d tx wasm instantiate 5 \
    --from MadeInTracker \
    --label "MadeInTracker chains-access-control" \
    --admin okp41wy8ywt98sv5pmsg873rct4pxtw0ntxuwxdtzen \
    --chain-id okp4-nemeton-1 \
    --gas 1000000 \
    --node https://api.testnet.okp4.network:443/rpc \
    "{\"program\":\"$(cat gov.pl | base64 | tr -d '\n')\", \"storage_address\": \"okp41lppz4x9dtmccek2m6cezjlwwzup6pdqrkvxjpk95806c3dewgrfq602kgx\"}"

txhash: 696754A57A957F4E343B3644B427467DA64E1F6DDAAC0DDDF9CA78FFBCC5AEF4

$ okp4d query txs --events 'message.sender=okp41wy8ywt98sv5pmsg873rct4pxtw0ntxuwxdtzen&instantiate.code_id=5' --chain-id okp4-nemeton-1 --node https://api.testnet.okp4.network:443/rpc --output json | jq '.txs[-1].logs[-1].events[] | select(.type == "instantiate").attributes[] | select(.key == "_contract_address").value'

"okp41cradz97s306zkrgh79jdj2m557te699lc0q35tkfu0ce6p9ue33qmgrlg0"
```

## Usage

Assuming the following shell function to simplify queries:
```zsh
function query_chains_access () {
    okp4d query wasm contract-state smart \
        okp41cradz97s306zkrgh79jdj2m557te699lc0q35tkfu0ce6p9ue33qmgrlg0 \
        --chain-id okp4-nemeton-1 \
        --node https://api.testnet.okp4.network:443/rpc \
        "{\"ask\": {\"query\": \"$1\"}}";
}
```

You can access the lists of authorized chains and roles:
```console
$ query_chains_access "help(chains, X)." | yq '.data.answer.results[0].substitutions[0].term.name'
"[okp4,cosmos,osmo]"

$ query_chains_access "help(roles, X)." | yq '.data.answer.results[0].substitutions[0].term.name'
"[guest,member,admin]"
```

You can verify if a DID from a cosmos chain has a role or not:
```console
$ query_chains_access "can('use_role', 'did:key:cosmos1s7pz4rc7sq4hj88ljcruvrh4v7hk28payxgnd5', member)." | yq '.data.answer.success'
false

$ query_chains_access "can('use_role', 'did:key:cosmos1s7pz4rc7sq4hj88ljcruvrh4v7hk28payxgnd5', guest)." | yq '.data.answer.success'
true

$ # between 7am and 12am UTC, okp4 DIDs are admin
$ query_chains_access "can('use_role', 'did:key:okp41wy8ywt98sv5pmsg873rct4pxtw0ntxuwxdtzen', admin)." | yq '.data.answer.success'
true
```

Finally, you can query what is the role of a DID this way:
```console
➜ query_chains_access "can('use_role', 'did:key:okp41wy8ywt98sv5pmsg873rct4pxtw0ntxuwxdtzen', Role)." | yq '.data.answer.results[0].substitutions[0] | {(.variable): .term.name}'
{
  "Role": "admin"
}
```

## Related documentation

- [Create a smart contract](https://docs.okp4.network/tutorials/cli-1#smart-contracts-operations)
- [Nemeton builder program](https://nemeton.okp4.network/builders/challenges#challenges)
- [Multiple source example](https://github.com/okp4/contracts/tree/v2.0.0/contracts/okp4-law-stone/examples/multiple-sources)
