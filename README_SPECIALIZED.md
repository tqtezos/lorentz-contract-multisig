
# Originated Example

You can find an originated example [here](https://better-call.dev/carthage/KT1NUp9a8gC5xQJDi4E9hr2WRZpUxx8BQrT3/operations).

## Specialized Multisig Contract

The specialized multisig contract is a variant of the generic multisig contract where:
- Tez may not be sent to the contract
- Only one parameter type may be signed and sent to the contract

Unless you're changing the keys/threshold, you sign and send:
  * A value of the specialized parameter's type
  * A target contract address, whose entrypoint matches the specialized parameter's type
  * The signatures
The contract will check the signatures, increment the counter, and call the 
target contract with the given parameter value.


### Parameter and storage types

```
parameter (pair (pair (nat :counter)
                      (or :action (pair nat (contract nat))
                                  (pair (nat :threshold)
                                        (list :keys key))))
                (list :sigs (option signature)))

storage (pair nat
              (pair nat
                    (list key)))
```

### Printing the contract

To print the contract, specialized to `nat`:

```bash
❯❯❯ stack exec -- lorentz-contract-multisig GenericMultisig print-specialized \
  --parameterType 'nat' --oneline
```

These `bash` functions are helpful to get a known user's public, secret keys, respectively:

```bash
get_public_key(){ tezos-client show address $1 2>/dev/null | tail -n 1 | cut -d " " -f 3;}
get_secret_key(){ tezos-client show address $1 -S 2>/dev/null | tail -n 1 | cut -d ":" -f 3;}
```

See the [Specialized Multisig Tutorial](https://assets.tqtezos.com/docs/token-contracts/multisig-specialized/1-multisig-specialized-intro/)
on the [assets site](https://assets.tqtezos.com/docs/intro/) for more detail.

