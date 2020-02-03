
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

Invalid call (wrong argument):

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

Invalid call (wrong user):

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $ADMIN42_ADDRESS --arg 42 --burn-cap 0.000001

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

To print the contract, specialized to `nat`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig print-specialized \
  --parameterType 'nat' --oneline
```

These `bash` functions are helpful to get a known user's public, secret keys, respectively:

```bash
get_public_key(){ tezos-client show address $1 2>/dev/null | tail -n 1 | cut -d " " -f 3;}
get_secret_key(){ tezos-client show address $1 -S 2>/dev/null | tail -n 1 | cut -d ":" -f 3;}
```

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
Current head: BM6Swqd2Jdtf (timestamp: 2020-02-03T18:56:48-00:00, validation: 2020-02-03T18:57:17-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 29235 units (will add 100 for safety)
Estimated storage: 1012 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooHThNQbMaWRJNP4DEV4qjaJxMmbTQWpyCMCudj4dz6AYZGymxA'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooHThNQbMaWRJNP4DEV4qjaJxMmbTQWpyCMCudj4dz6AYZGymxA to be included --confirmations 30 --branch BM6Swqd2JdtfosCKjzNKndaMUUJ1ADj5SeTvgT23bB8XvoBadBN
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00394
    Expected counter: 33121
    Gas limit: 29335
    Storage limit: 1032 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.00394
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,126) ... +ꜩ0.00394
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
          KT1LzZTwPSxUWcrffbn8swtCScNGDTKjqXaL
        Storage size: 755 bytes
        Paid storage size diff: 755 bytes
        Consumed gas: 29235
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.755
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1LzZTwPSxUWcrffbn8swtCScNGDTKjqXaL originated.
Contract memorized as MultisigNat.
```

```bash
❯❯❯ MULTISIG_NAT_ADDRESS="KT1LzZTwPSxUWcrffbn8swtCScNGDTKjqXaL"
```


Next, we originate a copy of the `admin_42` contract where the `MultisigNat` contract is the "admin":

```bash
❯❯❯ alpha-client --wait none originate contract MultisigAdmin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat admin_42.tz | tr '\n' ' ')" --init "\"$MULTISIG_NAT_ADDRESS\"" --burn-cap 0.406

Waiting for the node to be bootstrapped before injection...
Current head: BM4dXxNfze6j (timestamp: 2020-02-03T18:58:18-00:00, validation: 2020-02-03T18:58:51-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13516 units (will add 100 for safety)
Estimated storage: 406 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'op36LaDQS5bZNszGkGRf7ts7WncEBSKhmHmXb3Teoyt7t18QXan'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for op36LaDQS5bZNszGkGRf7ts7WncEBSKhmHmXb3Teoyt7t18QXan to be included --confirmations 30 --branch BM4dXxNfze6jRqchpAYnkajr8TRYCpWC63ymJgJUxZz8ja4bpNd
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001754
    Expected counter: 33122
    Gas limit: 13616
    Storage limit: 426 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001754
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,126) ... +ꜩ0.001754
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter nat ;
          ..
                 PAIR } }
        Initial storage: "KT1LzZTwPSxUWcrffbn8swtCScNGDTKjqXaL"
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Cy6mRw9U19GAkmcsLjj3sBezG3XBL9VZf
        Storage size: 149 bytes
        Paid storage size diff: 149 bytes
        Consumed gas: 13516
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.149
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1Cy6mRw9U19GAkmcsLjj3sBezG3XBL9VZf originated.
Contract memorized as MultisigAdmin42.
```

```bash
❯❯❯ MULTISIG_ADMIN42_ADDRESS="KT1Cy6mRw9U19GAkmcsLjj3sBezG3XBL9VZf"
```

Generate the `bytes` to sign:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig run-specialized \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS" --multisig-contract "$MULTISIG_NAT_ADDRESS" --counter 0 \
  --signatures "Nothing" \
  --signerKeys "[\"$(get_public_key bob)\"]"
"0x0507070a00000016018821643b501b2236a21871f3c3b5f52ce3c346ee000707000005050707002a0a00000016013019aebe083e0b201a3830d4075a3c8b715e03bc00"
```

Sign using the `tezos-client`:

```bash
❯❯❯ alpha-client sign bytes "0x0507070a00000016018821643b501b2236a21871f3c3b5f52ce3c346ee000707000005050707002a0a00000016013019aebe083e0b201a3830d4075a3c8b715e03bc00" for bob

Signature: edsigu58rM3JFCWKfsLFtETZGd81FC3b32FY3RU1DeAYNQiDGEGY2r3NUokzQEAbjscnZdtAiX7DHhj5UmYtKjUMkMpUxjxQ8x1
```

```bash
❯❯❯ OPERATION_SIGNATURE="edsigu58rM3JFCWKfsLFtETZGd81FC3b32FY3RU1DeAYNQiDGEGY2r3NUokzQEAbjscnZdtAiX7DHhj5UmYtKjUMkMpUxjxQ8x1"
```

Generate the signed parameter:

```bash
❯❯❯ ./stack exec -- lorentz-contract-multisig GenericMultisig run-specialized \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS" --multisig-contract "$MULTISIG_NAT_ADDRESS" --counter 0 \
  --signatures "Just[Just\"$OPERATION_SIGNATURE\"]" --signerKeys "[\"$(get_public_key bob)\"]"
Right (Pair (Pair 0 (Left (Pair 42 "KT1Cy6mRw9U19GAkmcsLjj3sBezG3XBL9VZf"))) { Some "edsigu58rM3JFCWKfsLFtETZGd81FC3b32FY3RU1DeAYNQiDGEGY2r3NUokzQEAbjscnZdtAiX7DHhj5UmYtKjUMkMpUxjxQ8x1" })
```

To submit the parameter:

```bash
❯❯❯  alpha-client --wait none transfer 0 from $BOB_ADDRESS to $MULTISIG_NAT_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-multisig GenericMultisig run-specialized \
  --target-parameterType 'nat' --target-parameter '42' \
  --target-contract "$MULTISIG_ADMIN42_ADDRESS" --multisig-contract "$MULTISIG_NAT_ADDRESS" --counter 0 \
  --signatures "Just[Just\"$OPERATION_SIGNATURE\"]" \
  --signerKeys "[\"$(get_public_key bob)\"]")" \
  --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLgSuao6BseZ (timestamp: 2020-02-03T19:08:28-00:00, validation: 2020-02-03T19:09:03-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 43691 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opKsgi6YLsM1xhn24qp7VdxuN8KATDq1oMqMLn7ZEJJtQMucjrV'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opKsgi6YLsM1xhn24qp7VdxuN8KATDq1oMqMLn7ZEJJtQMucjrV to be included --confirmations 30 --branch BLgSuao6BseZ9AE41DC6A77UJ1By8hpYwSgBkLPE9VbjsRijVSi
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.004802
    Expected counter: 33123
    Gas limit: 43791
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.004802
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,126) ... +ꜩ0.004802
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1LzZTwPSxUWcrffbn8swtCScNGDTKjqXaL
      Parameter: (Right
                    (Pair (Pair 0 (Left (Pair 42 "KT1Cy6mRw9U19GAkmcsLjj3sBezG3XBL9VZf")))
                          { Some "edsigu58rM3JFCWKfsLFtETZGd81FC3b32FY3RU1DeAYNQiDGEGY2r3NUokzQEAbjscnZdtAiX7DHhj5UmYtKjUMkMpUxjxQ8x1" }))
      This transaction was successfully applied
      Updated storage:
        (Pair 1
              (Pair 1
                    { 0x00622ace8f1d06165b951d0362624033e6f6eb5650c45290ff0ddbff6055d2caa1 }))
      Storage size: 755 bytes
      Consumed gas: 29909
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1LzZTwPSxUWcrffbn8swtCScNGDTKjqXaL
        To: KT1Cy6mRw9U19GAkmcsLjj3sBezG3XBL9VZf
        Parameter: 42
        This transaction was successfully applied
        Updated storage: 0x018821643b501b2236a21871f3c3b5f52ce3c346ee00
        Storage size: 149 bytes
        Consumed gas: 13782
```

