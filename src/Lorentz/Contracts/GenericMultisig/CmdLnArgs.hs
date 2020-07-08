{-# OPTIONS -Wno-missing-export-lists -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.GenericMultisig.CmdLnArgs where

import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Eq
import Data.Either
import Data.Function (id)
import Data.Functor
import Data.Functor.Identity
import Prelude (FilePath, IO, Ord(..), print, putStrLn, Bifunctor(..))
import Data.Maybe
import Data.Typeable

import Lorentz hiding (chainId, checkSignature, get)
import Lorentz.Contracts.IsKey
import Lorentz.Run (interpretLorentzInstr)

import Michelson.Interpret
import Michelson.Parser
import Michelson.Test.Dummy
import Michelson.Typed.Annotation
import Michelson.Typed.EntryPoints hiding (parseEpAddress)
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Tezos.Crypto (checkSignature)
import Util.IO
import Util.Named
import qualified Michelson.TypeCheck.Types as TypeCheck

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Base16 as Base16
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.GenericMultisig.Parsers
import Michelson.Typed.Value.Missing ()
import Michelson.Typed.Sing.Missing
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.SomeContractStorage
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G (parseTypeCheckValue)

import qualified Lorentz.Contracts.GenericMultisig as GenericMultisig
import qualified Lorentz.Contracts.GenericMultisig.Type as GenericMultisig

-- | Interpret Michelson code and generate corresponding bytestring.
packer :: forall key a p. (IsKey key, NicePackedValue a, NiceParameterFull p) => ChainId -> Address -> ((Natural, GenericMultisig.GenericMultisigAction key a)) -> ByteString
packer chainId' address' xs = either
    (error . fromString . show)
    (\case {Identity ys :& RNil -> ys})
    $ interpretLorentzInstr env (GenericMultisig.packWithChainId @key @_ @'[] @p) (Identity xs :& RNil)
  where
    env = dummyContractEnv { ceSelf = address', ceChainId = chainId' }

-- | Assume that the given `EpAddress` points to the contract root
unsafeRootContractRef :: ParameterScope cp => EpAddress -> ContractRef (Value cp)
unsafeRootContractRef EpAddress{..} =
  ContractRef eaAddress $
  SomeEpc $ EntryPointCall
  { epcName = eaEntryPoint
  , epcParamProxy = Proxy
  , epcLiftSequence = EplArgHere
  }



instance (SingI t) => ParameterHasEntryPoints (Value t) where
  type ParameterEntryPointsDerivation (Value t) = EpdNone

-- type IsComparable c = ToT c ~ 'Tc (ToCT c)
assertIsComparable ::
     forall (t :: T) a. SingI t
  => (( IsComparable (Value t)
      , SingI (ToCT (Value t))
      , Typeable (ToCT (Value t))
      ) =>
        a)
  -> a
assertIsComparable f =
  case sing @t of
    STc _ -> f
    _ -> error "assertIsComparable"

data CmdLnArgs
  = Print223
      { outputPath :: Maybe FilePath
      , oneline :: Bool
      }
  | PrintSpecialized
      { parameterType :: SomeSing T
      , outputPath :: Maybe FilePath
      , oneline :: Bool
      }
  | PrintWrapped
      { contractToWrap :: TypeCheck.SomeContract
      , outputPath :: Maybe FilePath
      , oneline :: Bool
      }
  | Init223
      { threshold :: Natural
      , signerKeyPairs :: [(PublicKey, PublicKey)]
      }
  | InitSpecialized
      { threshold :: Natural
      , signerKeys :: [PublicKey]
      }
  | InitWrapped
      { targetStorage :: SomeContractStorage
      , threshold :: Natural
      , signerKeys :: [PublicKey]
      }
  | GetCounterSpecialized
      { storageText :: Text
      , signerKeys :: [PublicKey]
      }
  | GetCounterWrapped
      { storageType :: SomeSing T
      , storageText :: Text
      , signerKeys :: [PublicKey]
      }
  | ChangeKeysMultisig
      { threshold :: Natural
      , newSignerKeys :: [PublicKey]
      , targetContract :: EpAddress
      , multisigContract :: Address
      , counter :: Natural
      , signatures :: Maybe [Maybe Signature]
      , signerKeys :: [PublicKey]
      , chainId :: ChainId
      }
  | RunMultisig
      { contractParameter :: SomeContractParam
      , targetContract :: EpAddress
      , multisigContract :: Address
      , counter :: Natural
      , signatures :: Maybe [Maybe Signature]
      , signerKeys :: [PublicKey]
      , chainId :: ChainId
      }

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ print223SubCmd
  , printSpecializedSubCmd
  , printWrappedSubCmd
  , init223SubCmd
  , initSpecializedSubCmd
  , initWrappedSubCmd
  , getCounterSpecializedSubCmd
  , getCounterWrappedSubCmd
  , changeKeysMultisigSubCmd
  , runMultisigSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    print223SubCmd =
      mkCommandParser "print-223"
      (Print223 <$> outputOptions <*> onelineOption)
      "Dump the 2/2/3 Multisig contract in form of Michelson code"

    printSpecializedSubCmd =
      mkCommandParser "print-specialized"
      (PrintSpecialized <$> parseSomeT "parameter" <*> outputOptions <*> onelineOption)
      "Dump the Specialized Multisig contract in form of Michelson code"

    printWrappedSubCmd =
      mkCommandParser "print-wrapped"
      (PrintWrapped <$>
        parseSomeContract "contractToWrap" <*>
        outputOptions <*>
        onelineOption
      )
      "Dump the Wrapped Multisig contract in form of Michelson code"

    init223SubCmd =
      mkCommandParser "init-223"
      (Init223 <$>
        parseNatural "threshold" <*>
        parseSignerKeyPairs "signerKeyPairs"
      )
      "Dump the intial storage for the Specialized Multisig contract"

    initSpecializedSubCmd =
      mkCommandParser "init-specialized"
      (InitSpecialized <$>
        parseNatural "threshold" <*>
        parseSignerKeys "signerKeys"
      )
      "Dump the intial storage for the Specialized Multisig contract"

    initWrappedSubCmd =
      mkCommandParser "init-wrapped"
      (InitWrapped <$>
        parseSomeContractStorage "targetStorage" <*>
        parseNatural "threshold" <*>
        parseSignerKeys "signerKeys"
      )
      "Dump the intial storage for the Wrapped Multisig contract"

    getCounterSpecializedSubCmd =
      mkCommandParser "get-counter-specialized"
      (GetCounterSpecialized <$>
        fmap T.pack (parseString "storageText") <*>
        parseSignerKeys "signerKeys"
      )
      ("Parse the storage for the Specialized Multisig contract, " <>
       "ensure the 'signerKeys' match, " <>
       "and return the current counter")

    getCounterWrappedSubCmd =
      mkCommandParser "get-counter-specialized"
      (GetCounterWrapped <$>
        parseSomeT "storage" <*>
        fmap T.pack (parseString "storageText") <*>
        parseSignerKeys "signerKeys"
      )
      ("Parse the storage for the Wrapped Multisig contract, " <>
       "ensure the 'signerKeys' match, " <>
       "and return the current counter")

    changeKeysMultisigSubCmd =
      mkCommandParser "change-keys-multisig"
      (ChangeKeysMultisig <$>
        parseNatural "threshold" <*>
        parseSignerKeys "newSignerKeys" <*>
        parseEpAddress "target-contract" <*>
        parseAddress "multisig-contract" <*>
        parseNatural "counter" <*>
        parseSignatures "signatures" <*>
        parseSignerKeys "signerKeys" <*>
        parseChainId "chainId"
      )
      "Dump the change keys parameter for the Specialized or Wrapped Multisig contract"

    runMultisigSubCmd =
      mkCommandParser "run-multisig"
      (RunMultisig <$>
        parseSomeContractParam "target-parameter" <*>
        parseEpAddress "target-contract" <*>
        parseAddress "multisig-contract" <*>
        parseNatural "counter" <*>
        parseSignatures "signatures" <*>
        parseSignerKeys "signerKeys" <*>
        parseChainId "chainId"
      )
      "Dump the run operation parameter for the Specialized or Wrapped Multisig contract"


infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Multisig contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print223 mOutput forceOneLine' ->
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
    GenericMultisig.generigMultisigContract223
  PrintSpecialized (SomeSing (st :: Sing t)) mOutput forceOneLine' ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    assertNestedBigMapsAbsense @t $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine' $
    GenericMultisig.specializedMultisigContract @(Value t) @PublicKey
  PrintWrapped wrappedContract mOutput oneline ->
    case wrappedContract of
      TypeCheck.SomeContract wrappedContractFC ->
        case wrappedContractFC of
          FullContract wrappedContractCode (_ :: ParamNotes cp) (_ :: Notes st) ->
            assertBigMapAbsense @cp $
            maybe TL.putStrLn writeFileUtf8 mOutput $
            printLorentzContract oneline $
            GenericMultisig.genericMultisigContractWrapper @(Value cp) @(Value st) @PublicKey
            (I wrappedContractCode)
  Init223 {..} ->
    if threshold > genericLength signerKeyPairs
       then error "threshold is greater than the number of signer keys"
       else TL.putStrLn $
         printLorentzValue @(GenericMultisig.Storage (PublicKey, PublicKey)) forceOneLine $
         ( #counter .! GenericMultisig.initialMultisigCounter
         , ( #threshold .! threshold
           , #keys      .! signerKeyPairs
           )
         )
  InitSpecialized {..} ->
    if threshold > genericLength signerKeys
       then error "threshold is greater than the number of signer keys"
       else TL.putStrLn $
         printLorentzValue @(GenericMultisig.Storage PublicKey) forceOneLine $
         ( #counter .! GenericMultisig.initialMultisigCounter
         , ( #threshold .! threshold
           , #keys      .! signerKeys
           )
         )
  InitWrapped (SomeContractStorage (initialWrappedStorage :: Value st)) threshold' signerKeys' ->
    TL.putStrLn $
    withDict (singTypeableT (sing @st)) $
    printLorentzValue @(Value st, GenericMultisig.Storage PublicKey) forceOneLine $
    ( initialWrappedStorage
    , ( #counter .! GenericMultisig.initialMultisigCounter
      , ( #threshold .! threshold'
        , #keys      .! signerKeys'
        )
      )
    )
  GetCounterSpecialized {..} ->
    let parsedStorage = parseNoEnv
          (G.parseTypeCheckValue @(ToT (GenericMultisig.Storage PublicKey)))
          "specialized-multisig"
          storageText
     in let (storedCounter, (_threshold, storedSignerKeys)) =
              either
                (error . T.pack . show)
                (fromVal @(GenericMultisig.Storage PublicKey))
                parsedStorage
         in if arg #keys storedSignerKeys == signerKeys
               then print storedCounter
               else do
                 putStrLn @Text "Stored signer keys:"
                 print signerKeys
                 error "Stored signer keys do not match provided signer keys"
  GetCounterWrapped (SomeSing (st :: Sing t)) storageText' signerKeys' ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    let parsedStorage = parseNoEnv
          (G.parseTypeCheckValue @(ToT (Value t, GenericMultisig.Storage PublicKey)))
          "specialized-multisig"
          storageText'
     in let (_, (storedCounter, (_threshold, storedSignerKeys))) =
              either
                (error . T.pack . show)
                (fromVal @(Value t, GenericMultisig.Storage PublicKey))
                parsedStorage
         in if arg #keys storedSignerKeys == signerKeys'
               then print storedCounter
               else do
                 putStrLn @Text "Stored signer keys:"
                 print signerKeys'
                 error "Stored signer keys do not match provided signer keys"
  ChangeKeysMultisig {..} ->
    let changeKeysParam = (counter, GenericMultisig.ChangeKeys @PublicKey @((), ContractRef ()) (#threshold .! threshold, #keys .! newSignerKeys))
        bytes = packer @PublicKey @((), ContractRef ()) @(GenericMultisig.ChangeKeyParams PublicKey) chainId multisigContract changeKeysParam
    in
    if threshold > genericLength newSignerKeys
       then error "threshold is greater than the number of signer keys"
       else
       case signatures of
         Nothing -> print . ("0x" <>) . Base16.encode $ bytes
         -- . lPackValue . asPackType @((), ContractRef ()) $ (toAddress targetContract, changeKeysParam)
         Just someSignatures ->
            if checkSignaturesValid bytes $ zip signerKeys someSignatures
               then
                 TL.putStrLn $
                 printLorentzValue @(GenericMultisig.MainParams PublicKey ((), ContractRef ())) forceOneLine $
                 asParameterType $
                 (bimap (#counter .!) (#action .!) changeKeysParam, #sigs .! someSignatures)
               else error "invalid signature(s) provided"
  RunMultisig {..} ->
    case contractParameter of
      SomeContractParam (param :: Value cp) _ (Dict, Dict) ->
        assertNestedBigMapsAbsense @cp $
        let runParam =
              ( counter
              , GenericMultisig.Operation
                  @PublicKey
                  @(Value cp, ContractRef (Value cp))
                  (param, unsafeRootContractRef @cp targetContract))
            bytes = packer
                   @PublicKey
                   @(Value cp, ContractRef (Value cp))
                   @(GenericMultisig.MainParams PublicKey (Value cp, ContractRef (Value cp)))
                   chainId multisigContract runParam
         in case signatures of
              Nothing -> print . ("0x" <>) . Base16.encode $ bytes
              Just someSignatures ->
                if checkSignaturesValid bytes $ zip signerKeys someSignatures
                   then
                     TL.putStrLn $
                     printLorentzValue @(GenericMultisig.MainParams PublicKey (Value cp, ContractRef (Value cp))) forceOneLine $
                     asParameterType $
                     (bimap (#counter .!) (#action .!) runParam, #sigs .! someSignatures)
                   else error "invalid signature(s) provided"
  where
    forceOneLine = True

    asParameterType :: forall cp. GenericMultisig.MainParams PublicKey cp -> GenericMultisig.MainParams PublicKey cp
    asParameterType = id

checkSignaturesValid :: ByteString -> [(PublicKey, Maybe Signature)] -> Bool
checkSignaturesValid = all . checkSignatureValid

checkSignatureValid :: ByteString -> (PublicKey, Maybe Signature) -> Bool
checkSignatureValid _ (_, Nothing) = True
checkSignatureValid a (pubKey, Just sig) = checkSignature pubKey sig a
