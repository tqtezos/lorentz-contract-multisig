
## Admin42 Contract

The `admin_42.tz` accepts a `nat` parameter (which must be `42`), only from the address in storage.
This is a good contract to test the specialized multisig.

```bash
❯❯❯ alpha-client --wait none originate contract Admin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat admin_42.tz | tr '\n' ' ')" --init "\"$BOB_ADDRESS\"" --burn-cap 0.406
```

```bash
❯❯❯ ADMIN42_ADDRESS="KT1LrmhejxwKB1mXiRyi9Bun5qRvBv6BUQvm"
```

Valid call:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to $ADMIN42_ADDRESS --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLUy1hNv2rcX (timestamp: 2020-01-31T18:54:06-00:00, validation: 2020-01-31T18:54:40-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13781 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooKBnZDrCf8GDp2LoQGp2KxSQMLB2JKKEkvSNGdPKVeH7RcKvNh'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooKBnZDrCf8GDp2LoQGp2KxSQMLB2JKKEkvSNGdPKVeH7RcKvNh to be included --confirmations 30 --branch BLYeCgYQjCnebgfJUhocMfRchuFAJx9rouLYeKt17wkuubKoG2a
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001646
    Expected counter: 33116
    Gas limit: 13881
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001646
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,123) ... +ꜩ0.001646
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1LrmhejxwKB1mXiRyi9Bun5qRvBv6BUQvm
      Parameter: 42
      This transaction was successfully applied
      Updated storage: 0x0000aad02222472cdf9892a3011c01caf6407f027081
      Storage size: 149 bytes
      Consumed gas: 13781
```

Invalid call:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to $ADMIN42_ADDRESS --arg 43 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLYeCgYQjCne (timestamp: 2020-01-31T18:54:36-00:00, validation: 2020-01-31T18:54:56-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 33116
    Gas limit: 800000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1LrmhejxwKB1mXiRyi9Bun5qRvBv6BUQvm
      Parameter: 43
      This operation FAILED.

Runtime error in contract KT1LrmhejxwKB1mXiRyi9Bun5qRvBv6BUQvm:
  01: { parameter nat ;
  02:   storage address ;
  03:   code { DUP ;
  04:          CDR ;
  05:          SENDER ;
  06:          ASSERT_CMPEQ ;
  07:          DUP ;
  08:          CAR ;
  09:          PUSH nat 42 ;
  10:          ASSERT_CMPEQ ;
  11:          CDR ;
  12:          NIL operation ;
  13:          PAIR } }
At line 10 characters 9 to 21,
script reached FAILWITH instruction
with Unit
Fatal error:
  transfer simulation failed
```


## Specialized Multisig Contract

To print the contract, specialized to `nat`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig print-specialized \
  --parameterType 'nat' --oneline
```

get_public_key(){ tezos-client show address $1 2>/dev/null | tail -n 1 | cut -d " " -f 3;}
get_secret_key(){ tezos-client show address $1 -S 2>/dev/null | tail -n 1 | cut -d ":" -f 3;}

To make the initial storage, e.g. for one admin:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig init-specialized \
  --threshold 1 --signerKeys "[\"$(get_public_key bob)\"]"
Pair 0 (Pair 1 { "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" })
```

If the `threshold` is set too low, an error is thrown:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig init-specialized \
  --threshold 0 --signerKeys "[\"$(get_public_key bob)\"]"
threshold is smaller than the number of signer keys
CallStack (from HasCallStack):
  error, called at src/Lorentz/Contracts/GenericMultisig/CmdLnArgs.hs:366:13 in lorentz-contract-multisig-0.1.0.0-DgLAc9ZP0CUC0oppqO7m0V:Lorentz.Contracts.GenericMultisig.CmdLnArgs
```

```bash
❯❯❯ alpha-client --wait none originate contract MultisigNat \
  transferring 0 from $BOB_ADDRESS running \
  "$(./stack exec -- lorentz-contract-multisig GenericMultisig \
  print-specialized --parameterType 'nat' --oneline)" \
  --init "$(./stack exec -- lorentz-contract-multisig GenericMultisig \
  init-specialized --threshold 1 \
  --signerKeys "[\"$(get_public_key bob)\"]")" --burn-cap 1.012

Waiting for the node to be bootstrapped before injection...
Current head: BLurqGshznEL (timestamp: 2020-01-31T19:04:36-00:00, validation: 2020-01-31T19:05:17-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 29235 units (will add 100 for safety)
Estimated storage: 1012 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onfgJ8Co3jjzwdJzzZmVGWyNq1HsXtLCanXC5N5WxGk6nHLWy2r'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onfgJ8Co3jjzwdJzzZmVGWyNq1HsXtLCanXC5N5WxGk6nHLWy2r to be included --confirmations 30 --branch BLurqGshznELj4SWcvtXV7tR4YBdSBxh62D7yKekms2BDzGJJbg
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00394
    Expected counter: 33117
    Gas limit: 29335
    Storage limit: 1032 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.00394
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,123) ... +ꜩ0.00394
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
        ..
                     PAIR } } }
        Initial storage:
          (Pair 0 (Pair 1 { "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" }))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT19V7gpzuhZPqRv8D9UyuXyQjgwKfto7Nv4
        Storage size: 755 bytes
        Paid storage size diff: 755 bytes
        Consumed gas: 29235
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.755
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT19V7gpzuhZPqRv8D9UyuXyQjgwKfto7Nv4 originated.
Contract memorized as MultisigNat.
```

```bash
❯❯❯ MULTISIG_NAT_ADDRESS="KT19V7gpzuhZPqRv8D9UyuXyQjgwKfto7Nv4" 
```


Next, we originate a copy of the `admin_42` contract where the `MultisigNat` contract is the "admin":

```bash
❯❯❯ alpha-client --wait none originate contract MultisigAdmin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat admin_42.tz | tr '\n' ' ')" --init "\"$MULTISIG_NAT_ADDRESS\"" --burn-cap 0.406

Waiting for the node to be bootstrapped before injection...
Current head: BMQ3n3snZL7C (timestamp: 2020-01-31T19:08:36-00:00, validation: 2020-01-31T19:08:46-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13516 units (will add 100 for safety)
Estimated storage: 406 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opFd2aTrUGJTjByw4gbQwPaEu8s3RLrEJ49u2DNYnGda9P4mQs8'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opFd2aTrUGJTjByw4gbQwPaEu8s3RLrEJ49u2DNYnGda9P4mQs8 to be included --confirmations 30 --branch BMQ3n3snZL7C6sdEzJEBw3RnnxYZ9N9eEYisp81dqEDe8vn8fkJ
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001754
    Expected counter: 33118
    Gas limit: 13616
    Storage limit: 426 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001754
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,123) ... +ꜩ0.001754
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter nat ;
          storage address ;
          code { DUP ;
                 ..
                 PAIR } }
        Initial storage: "KT19V7gpzuhZPqRv8D9UyuXyQjgwKfto7Nv4"
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1SfofioeonnDp2uJqTBFX9vqKm7GmTNJvz
        Storage size: 149 bytes
        Paid storage size diff: 149 bytes
        Consumed gas: 13516
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.149
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1SfofioeonnDp2uJqTBFX9vqKm7GmTNJvz originated.
Contract memorized as MultisigAdmin42.
```

```bash
❯❯❯ MULTISIG_ADMIN42_ADDRESS="KT1SfofioeonnDp2uJqTBFX9vqKm7GmTNJvz"
```

Generate the `bytes` to sign:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig run-specialized --target-parameterType 'nat' --target-parameter '42' --target-contract "$MULTISIG_ADMIN42_ADDRESS" --counter 0 --signatures "Nothing"
"0x0507070a0000001601c66619625055cd62fbca313511b53fcd4dae476100070700000505002a"
```

Sign using the `tezos-client`:

```bash
❯❯❯ alpha-client sign bytes "0x0507070a0000001601c66619625055cd62fbca313511b53fcd4dae476100070700000505002a" for bob

Signature: edsigtcxgU5MqVLocrbarx8knBiP5DNjwfF4A3z3mgp8gGjBZy8cpGuaBcXt2fRnH9kaadB4V6pyMQNBq9juks1xFTLQvoR7FY4
```

```bash
❯❯❯ OPERATION_SIGNATURE="edsigtcxgU5MqVLocrbarx8knBiP5DNjwfF4A3z3mgp8gGjBZy8cpGuaBcXt2fRnH9kaadB4V6pyMQNBq9juks1xFTLQvoR7FY4"
```

Generate the signed parameter:

```bash
❯❯❯ ❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig run-specialized \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS" --counter 0 \
  --signatures "Just[Just\"$OPERATION_SIGNATURE\"]"
Right (Pair (Pair 0 (Left 42)) { Some "edsigtcxgU5MqVLocrbarx8knBiP5DNjwfF4A3z3mgp8gGjBZy8cpGuaBcXt2fRnH9kaadB4V6pyMQNBq9juks1xFTLQvoR7FY4" })
```

To submit the parameter:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to $MULTISIG_NAT_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-multisig GenericMultisig run-specialized \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS" --counter 0 \
  --signatures "Just[Just\"$OPERATION_SIGNATURE\"]")" \
  --burn-cap 0.000001 --dry-run
```

