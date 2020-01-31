{-# OPTIONS -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.GenericMultisig.CmdLnArgs where

import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id)
import Prelude (FilePath, IO, Ord(..), print)
import Data.String (String)
import Data.Maybe
import Data.Typeable

import Lorentz hiding (get)
import Michelson.Parser
import Michelson.Typed.Annotation
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Michelson.Typed.Value
import Util.IO
import qualified Michelson.Untyped.Type as U

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.Util ()
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Parse
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G (parseTypeCheckValue)

import qualified Lorentz.Contracts.GenericMultisig as GenericMultisig
import qualified Lorentz.Contracts.GenericMultisig.Type as GenericMultisig

instance IsoCValue (Value ('Tc ct)) where
  type ToCT (Value ('Tc ct)) = ct
  toCVal (VC xs) = xs
  fromCVal = VC


assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

assertNestedBigMapsAbsense :: forall (t :: T) a. SingI t => (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapsAbsense f =
  case nestedBigMapsAbsense (sing @t) of
    Nothing -> error "assertNestedBigMapsAbsense"
    Just Dict -> forbiddenNestedBigMaps @t f

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

singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT SCInt = Dict
singTypeableCT SCNat = Dict
singTypeableCT SCString = Dict
singTypeableCT SCBytes = Dict
singTypeableCT SCMutez = Dict
singTypeableCT SCBool = Dict
singTypeableCT SCKeyHash = Dict
singTypeableCT SCTimestamp = Dict
singTypeableCT SCAddress = Dict

singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict

singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

-- | Make a type non-explicit
unExplicitType :: U.Type -> U.T
unExplicitType =
  \case
    U.Type t _ -> t

-- | Convert a `U.Comparable` to `CT`
fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

-- | Convert a `U.Type` to `T`
fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

-- | Convert a `U.T` to `T`
fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TChainId = TChainId
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

parseSignatures :: String -> Opt.Parser (Maybe [Maybe Signature])
parseSignatures name =
  (Opt.option (Just <$> Opt.auto) $ mconcat
    [ Opt.long name
    , Opt.metavar "Maybe [Maybe Signature]"
    , Opt.help "Ordered list of signatures, with Nothing for missing. Elide to dump the message to sign."
    ]) <|> pure Nothing

-- | Parse some `T`
parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          type_
          name
          typeStr
     in let type' = either (error . T.pack . show) unExplicitType parsedType
     in withSomeSingT (fromUntypedT type') SomeSing
  ) <$>
  Opt.strOption @Text
    (mconcat
      [ Opt.long $ name ++ "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " ++ name
      ])

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (G.parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])


data CmdLnArgs
  = PrintSpecialized
      { parameterType :: SomeSing T
      , outputPath :: Maybe FilePath
      , oneline :: Bool
      }
  | InitSpecialized
      { threshold :: Natural
      , signerKeys :: [PublicKey]
      }
  | ChangeKeysSpecialized
      { threshold :: Natural
      , signerKeys :: [PublicKey]
      , targetContract :: Address
      , counter :: Natural
      , signatures :: Maybe [Maybe Signature]
      }
  | RunSpecialized
      { contractParameter :: SomeContractParam
      , targetContract :: Address
      , counter :: Natural
      , signatures :: Maybe [Maybe Signature]
      }
  -- | PrintWrapper
  --     { parameterType :: SomeSing T
  --     , storageType :: SomeSing T
  --     , outputPath :: Maybe FilePath
  --     , oneline :: Bool
  --     }

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSpecializedSubCmd
  , initSpecializedSubCmd
  , changeKeysSpecializedSubCmd
  , runSpecializedSubCmd
  -- , printWrapperSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSpecializedSubCmd =
      mkCommandParser "print-specialized"
      (PrintSpecialized <$> parseSomeT "parameter" <*> outputOptions <*> onelineOption)
      "Dump the Specialized Multisig contract in form of Michelson code"

    initSpecializedSubCmd =
      mkCommandParser "init-specialized"
      (InitSpecialized <$>
        parseNatural "threshold" <*>
        parseSignerKeys "signerKeys"
      )
      "Dump the Specialized Multisig contract in form of Michelson code"

    changeKeysSpecializedSubCmd =
      mkCommandParser "change-keys-specialized"
      (ChangeKeysSpecialized <$>
        parseNatural "threshold" <*>
        parseSignerKeys "signerKeys" <*>
        parseAddress "target-contract" <*>
        parseNatural "counter" <*>
        parseSignatures "signatures"
      )
      "Dump the Specialized Multisig contract in form of Michelson code"

    runSpecializedSubCmd =
      mkCommandParser "run-specialized"
      (RunSpecialized <$>
        parseSomeContractParam "target-parameter" <*>
        parseAddress "target-contract" <*>
        parseNatural "counter" <*>
        parseSignatures "signatures"
      )
      "Dump the Specialized Multisig contract in form of Michelson code"

--     printWrapperSubCmd =
--       mkCommandParser "print-wrapper"
--       (PrintWrapper <$> parseSomeT "parameter" <*> parseSomeT "storage" <*> outputOptions <*> onelineOption)
--       "Dump the Specialized Multisig contract in form of Michelson code"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Multisig contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  PrintSpecialized (SomeSing (st :: Sing t)) mOutput forceOneLine ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    assertNestedBigMapsAbsense @t $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine (GenericMultisig.specializedMultisigContract @(Value t) @PublicKey)
  InitSpecialized {..} ->
    if threshold < genericLength signerKeys
       then error "threshold is smaller than the number of signer keys"
       else TL.putStrLn $
         printLorentzValue @(GenericMultisig.Storage PublicKey) forceOneLine $
         ( GenericMultisig.initialMultisigCounter
         , ( threshold
           , signerKeys
           )
         )
  ChangeKeysSpecialized {..} ->
    let changeKeysParam = (counter, GenericMultisig.ChangeKeys @PublicKey @() (threshold, signerKeys)) in
    if threshold < genericLength signerKeys
       then error "threshold is smaller than the number of signer keys"
       else
       case signatures of
         Nothing -> print . lPackValue $ (targetContract, changeKeysParam)
         Just someSignatures ->
           TL.putStrLn $
           printLorentzValue @(GenericMultisig.Parameter PublicKey ()) forceOneLine $
           GenericMultisig.MainParameter $
           (changeKeysParam, someSignatures)
  RunSpecialized {..} ->
    case contractParameter of
      SomeContractParam (param :: Value cp) _ (Dict, Dict) ->
        let runParam = (counter, GenericMultisig.Operation @PublicKey @(Value cp) param) in
        case signatures of
          Nothing -> print . lPackValue $ (targetContract, runParam)
          Just someSignatures ->
            TL.putStrLn $
            printLorentzValue @(GenericMultisig.Parameter PublicKey (Value cp)) forceOneLine $
            GenericMultisig.MainParameter $
            (runParam, someSignatures)
  where
    forceOneLine = True
  -- PrintWrapper (SomeSing (st1 :: Sing t1)) (SomeSing (st2 :: Sing t2)) mOutput forceOneLine ->
  --   withDict (singIT st) $
  --   withDict (singTypeableT st) $
  --   -- assertOpAbsense @t $
  --   -- assertBigMapAbsense @t $
  --   -- withDict (compareOpCT @(ToCT (Value t))) $
  --   maybe TL.putStrLn writeFileUtf8 mOutput $
  --   printLorentzContract forceOneLine (GenericMultisig.genericMultisigContractWrapper @(Value t1) @(Value t2) @PublicKey)

