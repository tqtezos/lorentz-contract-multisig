{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

-- | To use the contract:
--
-- @
--  stack build
--  stack exec -- lorentz-contracts print --name MultisigManagedLedgerAthens -o MultisigManagedLedgerAthens.tz
--  stack exec -- lorentz-contracts print --name ExplicitBigMapManagedLedgerAthens -o ExplicitBigMapManagedLedgerAthens.tz
--
--  alias alpha-client="tezos-client -A rpcalpha.tzbeta.net -P 443 -S"
--  alpha-client activate account fred with /Users/michaelklein/Downloads/tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir.json
--  alpha-client get balance for fred
--  FRED_ADDRESS="tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir"
--  _
-- @
--
module Lorentz.Contracts.GenericMultisig where

import Lorentz hiding (concat)
-- import Lorentz.Contracts.Util ()
import Michelson.Typed.Scope

import Lorentz.Contracts.IsKey
import Lorentz.Contracts.GenericMultisig.Type

import Prelude hiding ((>>), and, show, unwords, swap, drop)

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

-- | The default action: do nothing
genericMultisigContractDefault :: (b & s) :-> (([Operation], b) & s)
genericMultisigContractDefault = do
  nil @Operation
  pair

-- | Assert no token was sent:
-- to send tokens, the default entry point should be used
assertNoTokensSent :: s :-> s
assertNoTokensSent = do
  --   # Assert no token was sent:
  --   # to send tokens, the default entry point should be used
  --   PUSH mutez 0 ; AMOUNT ; ASSERT_CMPEQ ;
  let tokensSentOutsideDefault = [mt|Some tokens were sent to this contract outside of the default entry point.|]
  push (toMutez 0 :: Mutez) >> amount >> assertEq tokensSentOutsideDefault


-- | Pair the payload with the current contract address, to ensure signatures
-- | can't be replayed accross different contracts if a key is reused.
preparePayload :: forall key a b c s p. (IsKey key, NicePackedValue a, NiceParameterFull p)
  => Proxy p
  -> (((Natural, GenericMultisigAction key a), b) & (c & s)) :-> (c & (Natural & (ByteString & (b & (GenericMultisigAction key a & (c & s))))))
preparePayload _ = do
  --   SWAP ; DUP ; DIP { SWAP } ;
  swap >> dup >> dip swap

  -- DIP
  --   {
  --     UNPAIR ;
  --     # pair the payload with the current contract address, to ensure signatures
  --     # can't be replayed accross different contracts if a key is reused.
  --     DUP ; SELF ; ADDRESS ; PAIR ;
  --     PACK ; # form the binary payload that we expect to be signed
  --     DIP { UNPAIR @counter ; DIP { SWAP } } ; SWAP
  --   } ;
  dip $ do
    unpair
    dup >> packChainIdAndAddress @key @a @b @c @_ @p
      -- self @p
    dip (unpair @Natural >> dip swap) >> swap

packChainIdAndAddress
    :: forall key a b c s p. (IsKey key, NicePackedValue a, NiceParameterFull p)
    => ((Natural, GenericMultisigAction key a) & s) :-> (ByteString & s)
packChainIdAndAddress = selfCalling @p CallDefault >> address >> chainId >> pair >> pair >>  pack @((ChainId,Address), (Natural, GenericMultisigAction key a))


-- | `assertEq` on the parameter counter and storage counter
checkCountersMatch :: ((Natural, b) & (Natural & s)) :-> (b & s)
checkCountersMatch = do
  -- # Check that the counters match
  -- UNPAIR @stored_counter; DIP { SWAP };
  -- ASSERT_CMPEQ ;
  unpair >> dip swap
  -- getField #storedCounter
  let countersDoNotMatch = [mt|Counters do not match.|]
  assertEq countersDoNotMatch


-- | Compute the number of valid signatures
countValidSignatures :: forall key a s. IsKey key =>
     ((a, [Public key]) & (ByteString & (List (Maybe (Sig key)) & s))) :-> (a & (Natural & (List (Maybe (Sig key)) & (ByteString & s))))
countValidSignatures = do
  -- # Compute the number of valid signatures
  -- DIP { SWAP } ; UNPAIR @threshold @keys;
  dip swap >> unpair -- @Natural @[PublicKey]

  -- DIP
  --   {
  --     # Running count of valid signatures
  --     PUSH @valid nat 0; SWAP ;
  --     ITER
  --       {
  --         DIP { SWAP } ; SWAP ;
  --         IF_CONS
  --           {
  --             IF_SOME
  --               { SWAP ;
  --                 DIP
  --                   {
  --                     SWAP ; DIIP { DUUP } ;
  --                     # Checks signatures, fails if invalid
  --                     { DUUUP; DIP {CHECK_SIGNATURE}; SWAP; IF {DROP} {FAILWITH} };
  --                     PUSH nat 1 ; ADD @valid } }
  --               { SWAP ; DROP }
  --           }
  --           {
  --             # There were fewer signatures in the list
  --             # than keys. Not all signatures must be present, but
  --             # they should be marked as absent using the option type.
  --             FAIL
  --           } ;
  --         SWAP
  --       }
  --   } ;
  dip $ do
    push (0 :: Natural) >> swap
    iter $ do
      dip swap >> swap
      ifCons
        (ifSome
          (do
            swap
            dip $ do
              swap >> (dip $ dip $ duupX @2)
              -- duupX @3 >> dip checkSignature >> swap >> if_ drop (failCustom #invalidSignature)
              duupX @3 >> dip (checkKeySignature @key) >> swap >> if_ drop failWith
              push (1 :: Natural) >> add
          )
          (swap >> drop)
        )
        -- (failCustom #fewerSignaturesThanKeys)
        failWith
      swap


-- | Assert that the threshold is less than or equal to the
-- number of valid signatures.
assertQuorumPresent :: (Natural & (Natural & s)) :-> s
assertQuorumPresent = do
  let quorumNotPresent = [mt|Quorum not present|]
  -- # Assert that the threshold is less than or equal to the
  -- # number of valid signatures.
  -- ASSERT_CMPLE ;
  -- getField #threshold >>
  assertLe quorumNotPresent

-- | Assert no unchecked signature remains
assertAllSignaturesChecked :: forall key b c. IsKey key => ([Maybe (Sig key)] & (b & c)) :-> c
assertAllSignaturesChecked = do
  -- # Assert no unchecked signature remains
  -- IF_CONS {FAIL} {} ;
  -- DROP ;
  -- ifCons (failCustom #uncheckedSignaturesRemain) nop
  ifCons failWith nop
  drop


-- | Increment counter and place in storage
incrementAndStoreCounter :: (a & ((Natural, b) & s)) :-> (a & ((Natural, b) & s))
incrementAndStoreCounter = do
  -- # Increment counter and place in storage
  -- DIP { UNPAIR ; PUSH nat 1 ; ADD @new_counter ; PAIR} ;
  dip $ do
    unpair
    push (1 :: Natural)
    add
    pair

multisigSetup ::
     forall key a p. (IsKey key, NicePackedValue a, NiceParameterFull p)
  => Proxy p
  -> (((Natural, GenericMultisigAction key a), [Maybe (Sig key)]) & (( Natural
                                                                     , ( Natural
                                                                       , [Public key])) & '[]))
  :-> (GenericMultisigAction key a & (( Natural, ( Natural, [Public key])) & '[]))
multisigSetup p = do
  assertNoTokensSent
  preparePayload p
  checkCountersMatch
  countValidSignatures @key
  assertQuorumPresent
  assertAllSignaturesChecked @key
  incrementAndStoreCounter



-- | This is an extension of the generic multisig contract:
-- - It accepts an additional parameter: @a@
-- - It stores an additional parameter: @b@
-- - It accepts a `Lambda` from @(a, b)@ to a list of `Operation`s:
--   this is a static method of extending the contract
genericMultisigContractMain :: forall a b key p.
     (IsKey key, NicePackedValue a, NiceParameterFull p)
  => Proxy p
  -- -> (b & (a & '[])) :-> (List Operation & '[])
  -> (a & (b & '[])) :-> (List Operation & b & '[])
  -> '[ MainParams key a, (b, Storage key)] :-> '[ ([Operation], (b, Storage key))]
genericMultisigContractMain p runParam = do
  -- { # Main entry point
  dip unpair
  swap
  -- b on top of stack

  dip $ multisigSetup @key p
  swap >> dip swap

  -- # We have now handled the signature verification part,
  -- # produce the operation requested by the signers.
  -- IF_LEFT
  --   { # Get operation
  --     UNIT ; EXEC
  --   }
  --   {
  --     # Change set of signatures
  --     DIP { CAR } ; SWAP ; PAIR ; NIL operation
  --   };
  -- PAIR }
  caseT @(GenericMultisigAction key a)
    ( #cOperation /-> dip swap >> framed runParam -- (swap >> dip (swap >> dup >> dip runParam >> swap) >> swap >> dip swap)
    , #cChangeKeys /-> (dip car >> swap >> pair >> swap >> nil)
    )
  dip pair
  pair

-- | Given a method to run the parameter type, create a
-- multisig version of the method.
genericMultisigContract ::
     forall a b key p. (IsKey key, NicePackedValue a, NiceParameterFull p)
  => Proxy p
  -> (a & (b & '[])) :-> ([Operation] & b & '[]) -> ContractCode (Parameter key a) (b, Storage key)
genericMultisigContract p runParam = do
  unpair
  caseT @(Parameter key a)
    -- { # Default entry point: do nothing
    --   # This entry point can be used to send tokens to this contract
    --   DROP ; NIL operation ; PAIR }
    ( #cDefault /->
      genericMultisigContractDefault
      -- do
      -- nil @Operation
      -- pair
    , #cMainParameter /->
      genericMultisigContractMain p runParam
    )

-- | Given a contract, produce a multisig-wrapped contract, i.e. one where
-- each operation must be signed by at least the threshold number of signers
genericMultisigContractWrapper ::
     forall a b key. (IsKey key, NicePackedValue a, ForbidNestedBigMaps (ToT a), Typeable a, ParameterHasEntryPoints (Parameter key a))
  => ContractCode a b
  -> ContractCode (Parameter key a) (b, Storage key)
genericMultisigContractWrapper wrappedContract =
  genericMultisigContract @a @b @key (Proxy @(Parameter key a)) $ do
    pair
    wrappedContract
    unpair

-- | This is an extension of the generic multisig contract:
-- - It accepts an additional parameter: @a@
-- - It stores an additional parameter: @b@
-- - It accepts a `Lambda` from @(a, b)@ to a list of `Operation`s:
--   this is a static method of extending the contract
genericMultisigContractSimpleStorageMain :: forall a key p. (IsKey key, NicePackedValue a, NiceParameterFull p)
  => Proxy p
  -> (a & '[]) :-> ([Operation] & '[])
  -> '[ MainParams key a, Storage key] :-> '[ ([Operation], Storage key)]
genericMultisigContractSimpleStorageMain p runParam = do
  -- { # Main entry point
  multisigSetup @key p

  -- # We have now handled the signature verification part,
  -- # produce the operation requested by the signers.
  -- IF_LEFT
  --   { # Get operation
  --     UNIT ; EXEC
  --   }
  --   {
  --     # Change set of signatures
  --     DIP { CAR } ; SWAP ; PAIR ; NIL operation
  --   };
  -- PAIR }
  caseT @(GenericMultisigAction key a)
    ( #cOperation /-> (swap >> dip runParam >> swap)
    , #cChangeKeys /-> (dip car >> swap >> pair >> nil)
    )
  pair

-- | Given a method to run the parameter type, create a
-- multisig version of the method.
genericMultisigContractSimpleStorage ::
     forall a key p. (IsKey key, NicePackedValue a, NiceParameterFull p)
  => Proxy p
  -> (a & '[]) :-> ([Operation] & '[]) -> ContractCode (Parameter key a) (Storage key)
genericMultisigContractSimpleStorage p runParam = do
  unpair
  caseT @(Parameter key a)
    -- { # Default entry point: do nothing
    --   # This entry point can be used to send tokens to this contract
    --   DROP ; NIL operation ; PAIR }
    ( #cDefault /->
      genericMultisigContractDefault
    , #cMainParameter /->
      genericMultisigContractSimpleStorageMain p runParam
    )

-- | `genericMultisigContractSimpleStorage` specialized to accept
-- a contract call of a particular type, sans `Mutez`
specializedMultisigContract ::
     forall a key.
     ( IsKey key
     , NicePackedValue a
     , ForbidNestedBigMaps (ToT a)
     , Typeable a
     , ParameterHasEntryPoints (Parameter key (a, ContractRef a))
     )
  => ContractCode (Parameter key (a, ContractRef a)) (Storage key)
specializedMultisigContract =
  genericMultisigContractSimpleStorage @(a, ContractRef a) @key (Proxy @(Parameter key (a, ContractRef a))) $ do
    unpair
    dip $ do
      dip nil
      push $ toEnum @Mutez 0
    transferTokens
    cons

-- | Generic multisig contract with pairs of keys representing "individual" signers
generigMultisigContract223 ::
  ContractCode
    (Parameter (PublicKey, PublicKey) (Lambda () [Operation]))
    (Storage (PublicKey, PublicKey))
generigMultisigContract223 =
  genericMultisigContractSimpleStorage (Proxy @(Parameter (PublicKey, PublicKey) (Lambda () [Operation]))) $ do
    unit
    exec
