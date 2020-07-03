
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
