{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Lorentz.Contracts.GenericMultisig.Type where

import Lorentz hiding (concat)
-- import Lorentz.Contracts.Util ()

import Lorentz.Contracts.IsKey

import Fmt (Buildable(..), (+|), (|+))
import Data.ByteString.Internal (unpackChars)

import Text.Show (Show(..))

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

-- parameter (or (unit %default)
--               (pair %main
--                  (pair :payload
--                     (nat %counter) # counter, used to prevent replay attacks
--                     (or :action    # payload to sign, represents the requested action
--                        (lambda %operation unit (list operation))
--                        (pair %change_keys          # change the keys controlling the multisig
--                           (nat %threshold)         # new threshold
--                           (list %keys key))))     # new list of keys
--                  (list %sigs (option signature))));    # signatures

-- | @(threshold, keys)@
--
-- Note: @threshold@ is also known as @quorum@
type ChangeKeyParams key =
  ( Natural     -- "threshold" :!
  , [Public key] -- "keys"      :!
  )

-- | Either perform an `Operation` with the included contract or
-- use `ChangeKeys` to update the key list and threshold (quorum)
-- @
--  type GenericMultisigAction a = Either a ChangeKeyParams
-- @
data GenericMultisigAction key a
  = Operation !a
  | ChangeKeys !(ChangeKeyParams key)
  deriving stock Generic

deriving instance (IsKey key, Read a) => Read (GenericMultisigAction key a)
deriving instance (IsKey key, Show a) => Show (GenericMultisigAction key a)
deriving instance (IsKey key, IsoValue a) => IsoValue (GenericMultisigAction key a)

-- | @((counter, action), sigs)@
--
-- @
--  data MainParams = MainParams
--    { counter :: !Natural
--    , action  :: !GenericMultisigAction
--    , sigs    :: ![Maybe Signature]
--    }
--    deriving stock Generic
--    deriving anyclass IsoValue
-- @
type MainParams key a =
  ( ( Natural                 -- "counter" :!
    , GenericMultisigAction key a  -- "action"  :!
    )
  , [Maybe (Sig key)]       -- "sigs"    :!
  )

-- | Use `Default` to send tokens to the contract.
-- Otherwise, use `MainParameter`
data Parameter key a
  = Default
  | MainParameter (MainParams key a)
  deriving stock Generic

deriving instance (IsKey key, Read a) => Read (Parameter key a)
deriving instance (IsKey key, Show a) => Show (Parameter key a)
deriving instance (IsKey key, IsoValue a) => IsoValue (Parameter key a)

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

-- storage (pair (nat %stored_counter) (pair (nat %threshold) (list %keys key))) ;

-- | @(storedCounter, (threshold, keys))@
--
-- @
--  data Storage = Storage
--    { storedCounter :: !Natural
--    , threshold     :: !Natural
--    , keys          :: ![PublicKey]
--    } deriving stock Generic
--      deriving anyclass IsoValue
-- @
type Storage key =
  ( Natural -- "storedCounter" :!
  , ( Natural     -- "threshold" :!
    , [Public key] -- "keys"      :!
    )
  )

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | Attempt to change allowance from non-zero to a non-zero value.

-- | A provided `Signature` is invalid
type instance ErrorArg "invalidSignature" = ByteString

-- | There are fewer signatures than keys in the given parameters
type instance ErrorArg "fewerSignaturesThanKeys" = PublicKey
  -- ("required" :! Natural, "present" :! Natural)

-- | Unchecked signatures remain
type instance ErrorArg "uncheckedSignaturesRemain" = Maybe Signature

-- Buildable instances
----------------------------------------------------------------------------

instance Buildable ByteString where
  build = build . unpackChars

instance Buildable (CustomError "invalidSignature") where
  build (CustomError _ invalidSignature) =
    "Invalid signature: " +| invalidSignature |+ ""

instance Buildable (CustomError "fewerSignaturesThanKeys") where
  build (CustomError _ leftoverKey) =
    "Fewer signatures than keys: " +| leftoverKey |+ " is left over"
  -- build (CustomError _ (arg #required -> required, arg #present -> present)) =
  --   "Fewer signatures than keys, needed " +| required |+ ", but only" +|
  --   present |+ " is present"

instance Buildable (CustomError "uncheckedSignaturesRemain") where
  build (CustomError _ uncheckedSignatures) =
    "Unchecked signatures remain: " +| uncheckedSignatures |+ ""

-- Documentation
----------------------------------------------------------------------------

instance CustomErrorHasDoc "invalidSignature" where
  customErrDocMdCause =
    "A signature has been provided that does not match both the expected public \
    \key and data to be signed."
  customErrClass = ErrClassBadArgument

instance CustomErrorHasDoc "fewerSignaturesThanKeys" where
  customErrDocMdCause =
    "Fewer signatures than known public keys are present"
  customErrClass = ErrClassBadArgument

instance CustomErrorHasDoc "uncheckedSignaturesRemain" where
  customErrDocMdCause =
    "Unchecked signatures remain after validation"
  customErrClass = ErrClassContractInternal

