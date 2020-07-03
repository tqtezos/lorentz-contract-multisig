
# Originated Example

You can find an originated example [here](https://better-call.dev/babylon/KT1HmDgVFNM1t9sSYdURzUXWLUhb6AMyNxFW/operations).


## Admin42 Contract

The `admin_42.tz` accepts a `nat` parameter (which must be `42`), only from the address in storage.
This is a good contract to test the specialized multisig.

```bash
❯❯❯ tezos-client --wait none originate contract Admin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat admin_42.tz | tr '\n' ' ')" --init "\"$BOB_ADDRESS\"" --burn-cap 0.406
```

```bash
❯❯❯ ADMIN42_ADDRESS="KT1LrmhejxwKB1mXiRyi9Bun5qRvBv6BUQvm"
```

Valid call:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $ADMIN42_ADDRESS --arg 42 --burn-cap 0.000001

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

Invalid call (wrong argument):

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $ADMIN42_ADDRESS --arg 43 --burn-cap 0.000001

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

Invalid call (wrong user):

```bash
❯❯❯ tezos-client --wait none transfer 0 from $ALICE_ADDRESS to $ADMIN42_ADDRESS --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BKvAECjia3c4 (timestamp: 2020-02-03T19:06:18-00:00, validation: 2020-02-03T19:06:31-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0
    Expected counter: 125197
    Gas limit: 800000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1LrmhejxwKB1mXiRyi9Bun5qRvBv6BUQvm
      Parameter: 42
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
At line 6 characters 9 to 21,
script reached FAILWITH instruction
with Unit
Fatal error:
  transfer simulation failed
```

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
parameter (pair (pair nat
                      (or (pair nat
                                (contract nat))
                          (pair nat
                                (list key))))
                (list (option signature)));

storage (pair nat
              (pair nat
                    (list key)));
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

To make the initial storage, e.g. for one admin:

```bash
❯❯❯ stack exec -- lorentz-contract-multisig GenericMultisig init-specialized \
  --threshold 1 --signerKeys "[\"$(get_public_key bob)\"]"
new - Pair 0 (Pair 1 { "edpkurPsQ8eUApnLUJ9ZPDvu98E8VNj4KtJa1aZr16Cr5ow5VHKnz4" })


Pair 0 (Pair 1 { "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" })
```

If the `threshold` is set higher than the number of admins that can sign, an error is thrown:

```bash
❯❯❯ stack exec -- lorentz-contract-multisig GenericMultisig init-specialized  \ --threshold 2 --signerKeys "[\"$(get_public_key bob)\"]"

threshold is greater than the number of signer keys
CallStack (from HasCallStack):
  error, called at src/Lorentz/Contracts/GenericMultisig/CmdLnArgs.hs:376:13 in lorentz-contract-multisig-0.1.0.0-BUWXHalK6PtIEryQAwm6u2:Lorentz.Contracts.GenericMultisig.CmdLnArgs
```

```bash
❯❯❯ tezos-client --wait none originate contract MultisigNat \
  transferring 0 from $EBT_ADDRESS running \
  "$(stack exec -- lorentz-contract-multisig GenericMultisig \
  print-specialized --parameterType 'nat' --oneline)" \
  --init "$(stack exec -- lorentz-contract-multisig GenericMultisig \
  init-specialized --threshold 1 \
  --signerKeys "["$(get_public_key bob)"]")" --burn-cap 1.096

Waiting for the node to be bootstrapped before injection...
Current head: BKm3LmKTQv7T (timestamp: 2020-06-02T18:44:09-00:00, validation: 2020-06-02T18:44:35-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 31369 units (will add 100 for safety)
Estimated storage: 1096 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooKpPDGu1nK2eMYAFf4r83tAaSRrdXJ8bCEKnVj7z9f1GMYjEYc'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooKpPDGu1nK2eMYAFf4r83tAaSRrdXJ8bCEKnVj7z9f1GMYjEYc to be included --confirmations 30 --branch BKm3LmKTQv7TmmJAa5jPhg82ZboGyPompQxxhqMZzno1Qjn3NQK
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
    Fee to the baker: ꜩ0.004237
    Expected counter: 802103
    Gas limit: 31469
    Storage limit: 1116 bytes
    Balance updates:
      tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ............. -ꜩ0.004237
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,222) ... +ꜩ0.004237
    Origination:
      From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
      Credit: ꜩ0
      Script:
        { parameter
            (or (unit %default)
                (pair %mainParameter
                   (pair nat (or (pair nat (contract nat)) (pair nat (list key))))
                   (list (option signature)))) ;
          storage (pair nat (pair nat (list key))) ;
          code { CAST (pair (or unit
                                (pair (pair nat (or (pair nat (contract nat)) (pair nat (list key)))) (list (option signature))))
                            (pair nat (pair nat (list key)))) ;
                 DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DROP ; NIL operation ; PAIR }
                   { PUSH mutez 0 ;
                     AMOUNT ;
                     COMPARE ;
                     EQ ;
                     IF {}
                        { PUSH string
                               "Some tokens were sent to this contract outside of the default entry point." ;
                          FAILWITH } ;
                     SWAP ;
                     DUP ;
                     DIP { SWAP } ;
                     DIP { DUP ;
                           CAR ;
                           DIP { CDR } ;
                           DUP ;
                           SELF ;
                           ADDRESS ;
                           CHAIN_ID ;
                           PAIR ;
                           PAIR ;
                           PACK ;
                           DIP { DUP ; CAR ; DIP { CDR } ; DIP { SWAP } } ;
                           SWAP } ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIP { SWAP } ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "Counters do not match." ; FAILWITH } ;
                     DIP { SWAP } ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIP { PUSH nat 0 ;
                           SWAP ;
                           ITER { DIP { SWAP } ;
                                  SWAP ;
                                  IF_CONS
                                    { IF_NONE
                                        { SWAP ; DROP }
                                        { SWAP ;
                                          DIP { SWAP ;
                                                DIP { DIP { DIP { DUP } ; SWAP } } ;
                                                DIP 2 { DUP } ;
                                                DIG 2 ;
                                                DIP { CHECK_SIGNATURE } ;
                                                SWAP ;
                                                IF { DROP } { FAILWITH } ;
                                                PUSH nat 1 ;
                                                ADD } } }
                                    { FAILWITH } ;
                                  SWAP } } ;
                     COMPARE ;
                     LE ;
                     IF {} { PUSH string "Quorum not present" ; FAILWITH } ;
                     IF_CONS { FAILWITH } {} ;
                     DROP ;
                     DIP { DUP ; CAR ; DIP { CDR } ; PUSH nat 1 ; ADD ; PAIR } ;
                     IF_LEFT
                       { SWAP ;
                         DIP { DUP ;
                               CAR ;
                               DIP { CDR } ;
                               DIP { DIP { NIL operation } ; PUSH mutez 0 } ;
                               TRANSFER_TOKENS ;
                               CONS } ;
                         SWAP }
                       { DIP { CAR } ; SWAP ; PAIR ; NIL operation } ;
                     PAIR } } }
        Initial storage:
          (Pair 0 (Pair 1 { "edpkurPsQ8eUApnLUJ9ZPDvu98E8VNj4KtJa1aZr16Cr5ow5VHKnz4" }))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1TT1KY7MgfD3RNfj2zufn7uNTXx7PNTrFg
        Storage size: 839 bytes
        Paid storage size diff: 839 bytes
        Consumed gas: 31369
        Balance updates:
          tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ... -ꜩ0.839
          tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ... -ꜩ0.257

New contract KT1TT1KY7MgfD3RNfj2zufn7uNTXx7PNTrFg originated.
Contract memorized as MultisigNat.
```

```bash
MULTISIG_NAT_ADDRESS="KT1TT1KY7MgfD3RNfj2zufn7uNTXx7PNTrFg"
```

Next, we originate a copy of the `admin_42` contract where the `MultisigNat` contract is the "admin":

```bash
❯❯❯ tezos-client --wait none originate contract MultisigAdmin42 \
  transferring 0 from $EBT_ADDRESS running \
  "$(cat admin_42.tz | tr '\n' ' ')" --init "\"$MNADD\"" --burn-cap 0.406

Waiting for the node to be bootstrapped before injection...
Current head: BKtWUGLsoSgP (timestamp: 2020-06-02T18:46:39-00:00, validation: 2020-06-02T18:46:47-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13516 units (will add 100 for safety)
Estimated storage: 406 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opRXLQpVnKmAGFtfBhzTmWGAukDwF1R3hoB7X4tJNrNDGNj2Bj5'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opRXLQpVnKmAGFtfBhzTmWGAukDwF1R3hoB7X4tJNrNDGNj2Bj5 to be included --confirmations 30 --branch BKtWUGLsoSgPqFqrr21A5MXQtik9eoLaP6X5YUqpZWNYusFQgxL
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
    Fee to the baker: ꜩ0.001754
    Expected counter: 802104
    Gas limit: 13616
    Storage limit: 426 bytes
    Balance updates:
      tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ............. -ꜩ0.001754
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,222) ... +ꜩ0.001754
    Origination:
      From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
      Credit: ꜩ0
      Script:
        { parameter nat ;
          storage address ;
          code { DUP ;
                 CDR ;
                 SENDER ;
                 ASSERT_CMPEQ ;
                 DUP ;
                 CAR ;
                 PUSH nat 42 ;
                 ASSERT_CMPEQ ;
                 CDR ;
                 NIL operation ;
                 PAIR } }
        Initial storage: "KT1TT1KY7MgfD3RNfj2zufn7uNTXx7PNTrFg"
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1FkSWrMWyvmZUP7JfU2sZVNLbFw7V7KrZu
        Storage size: 149 bytes
        Paid storage size diff: 149 bytes
        Consumed gas: 13516
        Balance updates:
          tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ... -ꜩ0.149
          tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ... -ꜩ0.257

New contract KT1FkSWrMWyvmZUP7JfU2sZVNLbFw7V7KrZu originated.
Contract memorized as MultisigAdmin42.
```

```bash
MULTISIG_ADMIN42_ADDRESS="KT1FkSWrMWyvmZUP7JfU2sZVNLbFw7V7KrZu"
```

Before we generate the bytes we sign we first grab the chain id:

```bash
❯❯❯ tezos-client rpc get /chains/main/chain_id
"NetXjD3HPJJjmcd"
```

```bash
CHAINID="NetXjD3HPJJjmcd"
```

Generate the `bytes` to sign:

```bash
❯❯❯ stack exec -- lorentz-contract-multisig GenericMultisig \
  run-multisig  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS"" --multisig-contract "$MULTISIG_NAT_ADDRESS" \
  --counter 0  --signatures "Nothing"  --signerKeys "["$(get_public_key bob)"]" --chainId $CHAINID
"0x05070707070a0000001601cef28b459e586a9af4578cd00cadc78dcb012450000a000000049caecab90707000005050707002a0a00000016014e9d2344cc51d2033536c49be18b927906bc21cd00"
```

Sign using the `tezos-client`:

```bash
❯❯❯ tezos-client sign bytes "0x05070707070a0000001601cef28b459e586a9af4578cd00cadc78dcb012450000a000000049caecab90707000005050707002a0a00000016014e9d2344cc51d2033536c49be18b927906bc21cd00" for bob

Signature: edsigtyWE5aQh4sZeYern6xA4mZ61gmFXgLivFfL7UHdo5wERn8rDzA59xnGL7Wpfa3Byz1MYUqhBtR4RKqaDDmqKKcL9quGa6L
```

```bash
❯❯❯ OPERATION_SIGNATURE="edsigtyWE5aQh4sZeYern6xA4mZ61gmFXgLivFfL7UHdo5wERn8rDzA59xnGL7Wpfa3Byz1MYUqhBtR4RKqaDDmqKKcL9quGa6L"
```

Generate the signed parameter:

```bash
❯❯❯ stack exec -- lorentz-contract-multisig GenericMultisig run-multisig \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS"" --multisig-contract "$MULTISIG_NAT_ADDRESS" --counter 0 \
  --signatures "Just[Just\"$OPERATION_SIGNATURE\"]" --signerKeys "["$(get_public_key bob)"]" --chainId $CHAINID
Pair (Pair 0 (Left (Pair 42 "KT1FkSWrMWyvmZUP7JfU2sZVNLbFw7V7KrZu"))) { Some "edsigtyWE5aQh4sZeYern6xA4mZ61gmFXgLivFfL7UHdo5wERn8rDzA59xnGL7Wpfa3Byz1MYUqhBtR4RKqaDDmqKKcL9quGa6L" }
```

To submit the parameter:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $MULTISIG_NAT_ADDRESS \
  --arg "$(stack exec -- lorentz-contract-multisig GenericMultisig run-multisig \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS"" --multisig-contract "$MULTISIG_NAT_ADDRESS" --counter 0 \
  --signatures "Just[Just\"$OPERATION_SIGNATURE\"]" \
  --signerKeys "[\"$(get_public_key bob)\"]" \
  --chainId $CHAINID)"
  --burn-cap 0.000001 --entrypoint mainParameter

Waiting for the node to be bootstrapped before injection...
Current head: BMbpbDRbBGKm (timestamp: 2020-06-02T19:46:01-00:00, validation: 2020-06-02T19:46:31-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 45454 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'onxt9ZJ2Jso5JQdfpNoHH1zwCY1aKekBfKVq2gqBWfKvjpHUmTi'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onxt9ZJ2Jso5JQdfpNoHH1zwCY1aKekBfKVq2gqBWfKvjpHUmTi to be included --confirmations 30 --branch BLnu1Js9zZokypNxmdpm3f4Cq7KwRsYeJZpNk1M5cP9axeTW1kB
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
    Fee to the baker: ꜩ0.00499
    Expected counter: 802105
    Gas limit: 45554
    Storage limit: 0 bytes
    Balance updates:
      tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf ............. -ꜩ0.00499
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,222) ... +ꜩ0.00499
    Transaction:
      Amount: ꜩ0
      From: tz1RyrvobLvJcYJxZJ82JMN4EsrrNdn6f9Hf
      To: KT1TT1KY7MgfD3RNfj2zufn7uNTXx7PNTrFg
      Entrypoint: mainParameter
      Parameter: (Pair (Pair 0 (Left (Pair 42 "KT1FkSWrMWyvmZUP7JfU2sZVNLbFw7V7KrZu")))
                       { Some "edsigu4UBqDSDCfmoGfad4w7HgXBwrM3biUD3Qd3EuUP9J3qB6JtDrmQ4jCAs1DgNgq661sNhpM4ZxQ1rfccL5q9iTnY7iGPFr3" })
      This transaction was successfully applied
      Updated storage:
        (Pair 1
              (Pair 1
                    { 0x009f55199bf631608315c61a0692c1677ea8c941ae6c4539449577c627700053f9 }))
      Storage size: 839 bytes
      Consumed gas: 31672
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1TT1KY7MgfD3RNfj2zufn7uNTXx7PNTrFg
        To: KT1FkSWrMWyvmZUP7JfU2sZVNLbFw7V7KrZu
        Parameter: 42
        This transaction was successfully applied
        Updated storage: 0x01cef28b459e586a9af4578cd00cadc78dcb01245000
        Storage size: 149 bytes
        Consumed gas: 13782
```
