{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.IsKey where

import Lorentz hiding (concat)
import Tezos.Crypto.Orphans ()
import Michelson.Typed.Scope
import qualified Tezos.Crypto as Crypto
import qualified Tezos.Crypto.Ed25519 as Ed25519

import Data.Aeson hiding (String)

import Prelude hiding ((>>), and, show, unwords, swap)
import Text.Show (Show(..))
import Data.List (unwords, concat)
import Data.Kind
import qualified GHC.Base as Base
import qualified Control.Monad as Monad


-- | Some `Public` key
data SomePublicKey where
  SomePublicKey :: forall key. IsKey key => Proxy key -> Public key -> SomePublicKey

-- | Keys have "value-like" instances, e.g. `Typeable`, `Ord`, `Show`, `IsoValue`, etc.,
-- public keys, partial and total signatures, and ways to make and check signatures
class ( Typeable key
      , Ord (Public key)
      , Read (Public key)
      , Show (Public key)
      , ToJSON (Public key)
      , ToJSONKey (Public key)
      , FromJSON (Public key)
      , FromJSONKey (Public key)
      , IsoValue (Public key)
      , KnownValue (Public key)
      , NiceParameter (Public key)
      , HasNoOp (ToT (Public key))
      , HasNoBigMap (ToT (Public key))
      , HasNoNestedBigMaps (ToT (Public key))
      , Eq (PartialSig key)
      , Show (PartialSig key)
      , ToJSON (PartialSig key)
      , FromJSON (PartialSig key)
      , Eq (Sig key)
      , Read (Sig key)
      , Show (Sig key)
      , FromJSON (Sig key)
      , ToJSON (Sig key)
      , IsoValue (Sig key)
      , KnownValue (Sig key)
      , NiceParameter (Sig key)
      , HasNoOp (ToT (Sig key))
      , HasNoBigMap (ToT (Sig key))
      , HasNoNestedBigMaps (ToT (Sig key))
      ) => IsKey (key :: Type) where
  -- | The public key
  type Public key :: Type
  -- | A partial signature
  type PartialSig key :: Type
  -- | A complete signature
  type Sig key :: Type

  -- | Check the signature in Michelson
  checkKeySignature :: forall s. (Public key & (Sig key & (ByteString & s))) :-> (Bool & s)
  -- | Check the signature in Haskell
  checkKeySignatureHaskell :: Public key -> Sig key -> ByteString -> Either String ()

  -- | An empty `PartialSig`
  partialSig :: PartialSig key
  -- | Sign a `PartialSig` with the given `Ed25519.SecretKey`
  signWithKey :: Ed25519.SecretKey -> ByteString -> PartialSig key -> Either Base.String (PartialSig key)
  -- | Complete a `PartialSig` or return an error explaining how it's incomplete
  completeSig :: PartialSig key -> Either Base.String (Sig key)


instance IsKey PublicKey where
  type Public PublicKey = PublicKey
  type PartialSig PublicKey = Maybe Signature
  type Sig PublicKey = Signature
  checkKeySignature = checkSignature
  checkKeySignatureHaskell publicKey sig bytes' =
    bool
       (Left "Checking the signature failed")
       (return ()) $
       Crypto.checkSignature publicKey sig bytes'

  partialSig = Nothing

  signWithKey secretKey bytes Nothing =
    let signature = Ed25519.sign secretKey bytes -- Crypto.SignatureEd25519 $
     in let publicKey = Ed25519.toPublic secretKey
         in bool
              (Left $ "Failed to check signature: " Base.++ show signature)
              (Right $ Just $ Crypto.SignatureEd25519 signature)
              (Ed25519.checkSignature publicKey signature bytes)
  signWithKey _ _ (Just sig) =
    Left $ "signWithKey @PublicKey: already signed " Base.++ show sig

  completeSig Nothing = Left "incomplete"
  completeSig (Just sig) = Right sig

-- | A pair of partial signatures
data PartialSigPair a b =
    EmptySigPair
  | PartialSigL (PartialSig a)
  | PartialSigLR (Sig a) (PartialSig b)
  | PartialSigR (PartialSig b)
  | PartialSigRL (PartialSig a) (Sig b)
  | SigPair (Sig a) (Sig b)
  deriving (Generic)

deriving instance (IsKey a, IsKey b) => Eq (PartialSigPair a b)
deriving instance (IsKey a, IsKey b) => Show (PartialSigPair a b)
deriving instance (IsKey a, IsKey b) => FromJSON (PartialSigPair a b)
deriving instance (IsKey a, IsKey b) => ToJSON (PartialSigPair a b)

instance (IsKey a, IsKey b) => IsKey (a, b) where
  type Public (a, b) = (Public a, Public b)
  -- type Secret (a, b) = (Secret a, Secret b)
  type PartialSig (a, b) = PartialSigPair a b
  type Sig (a, b) = (Sig a, Sig b)
  checkKeySignature = do
    pair
    dup
    dip $ do
      unpair
      car
      dip $ do
        car
        dip dup
      checkKeySignature @a
    swap
    dip $ do
      unpair
      cdr
      dip cdr
      checkKeySignature @b
    and

  checkKeySignatureHaskell ~(publicKeyA, publicKeyB) ~(sigA, sigB) bytes' =
    (first (\str -> concat [show publicKeyA, ": ", str]) $ checkKeySignatureHaskell @a publicKeyA sigA bytes') Monad.>>
    (first (\str -> concat [show publicKeyB, ": ", str]) $ checkKeySignatureHaskell @b publicKeyB sigB bytes')

  partialSig = EmptySigPair

  signWithKey secretKey bytes EmptySigPair =
    case signWithKey @a secretKey bytes $ partialSig @a of
      Left errA ->
        case signWithKey @b secretKey bytes $ partialSig @b of
          Left errB -> Left $ "Neither key worked: " <> unwords [errA, errB]
          Right psigB ->
            case completeSig @b psigB of
              Left _ ->
                return $ PartialSigR @a @b psigB
              Right sigB ->
                return $ PartialSigRL @a @b (partialSig @a) sigB
      Right psigA ->
        case completeSig @a psigA of
          Left _ ->
            return $ PartialSigL @a @b psigA
          Right sigA ->
            return $ PartialSigLR @a @b sigA (partialSig @b)
  signWithKey secretKey bytes (PartialSigL psigA) =
    case signWithKey @a secretKey bytes psigA of
      Left err -> Left err
      Right psigA' ->
        case completeSig @a psigA' of
          Left _ ->
            return $ PartialSigL @a @b psigA'
          Right sigA ->
            return $ PartialSigLR @a @b sigA (partialSig @b)
  signWithKey secretKey bytes (PartialSigR psigB) =
    case signWithKey @b secretKey bytes psigB of
      Left err -> Left err
      Right psigB' ->
        case completeSig @b psigB' of
          Left _ ->
            return $ PartialSigR @a @b psigB'
          Right sigB ->
            return $ PartialSigRL @a @b (partialSig @a) sigB
  signWithKey secretKey bytes (PartialSigLR sigA psigB) =
    case signWithKey @b secretKey bytes psigB of
      Left err -> Left err
      Right psigB' ->
        case completeSig @b psigB' of
          Left _ ->
            return $ PartialSigLR @a @b sigA psigB'
          Right sigB ->
            return $ SigPair @a @b sigA sigB
  signWithKey secretKey bytes (PartialSigRL psigA sigB) =
    case signWithKey @a secretKey bytes psigA of
      Left err -> Left err
      Right psigA' ->
        case completeSig @a psigA' of
          Left _ ->
            return $ PartialSigRL @a @b psigA' sigB
          Right sigA ->
            return $ SigPair @a @b sigA sigB
  signWithKey _ _ (SigPair sigA sigB) =
    Left $ "signWithKey @(a, b): all signatures have been provided: " Base.++ show (sigA, sigB)

  completeSig EmptySigPair = Left "incomplete: EmptySigPair"
  completeSig (PartialSigL _) = Left "incomplete: PartialSigL"
  completeSig (PartialSigR _) = Left "incomplete: PartialSigR"
  completeSig (PartialSigLR _ _) = Left "incomplete: PartialSigLR"
  completeSig (PartialSigRL _ _) = Left "incomplete: PartialSigRL"
  completeSig (SigPair sigA sigB) = Right (sigA, sigB)

