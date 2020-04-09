{-# OPTIONS -Wno-orphans -Wno-missing-export-lists #-}

module Lorentz.Contracts.GenericMultisig.Parsers where

import Prelude (FilePath, ($), Natural, error)
import Data.Char
import Data.Bool
import Data.Either
import Data.String
import Data.Maybe
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read
import Text.Show
import Data.Function
import Data.Proxy
import qualified Text.ParserCombinators.ReadP as P

import Tezos.Crypto

import qualified Tezos.Crypto.Ed25519 as Ed25519
import qualified Tezos.Crypto.Secp256k1 as Secp256k1
import qualified Tezos.Crypto.P256 as P256

import qualified Tezos.Address as Tezos
import Lorentz (PublicKey, Address, View(..), GetDefaultEntryPointArg, NiceParameterFull, TAddress(..), callingDefTAddress)
import Michelson.Typed.T
import qualified Michelson.TypeCheck.Types as TypeCheck
import Michelson.Typed.Annotation
import Michelson.Typed.Sing
import Michelson.Typed.EntryPoints (EpAddress(..))
import qualified Michelson.Typed.EntryPoints as EntryPoints
import Michelson.Parser
import Michelson.TypeCheck
import Michelson.Macro

import Text.Megaparsec (parse)
import qualified Options.Applicative as Opt
import qualified Data.Text as T
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.GenericMultisig.Wrapper (parseTypeCheckValue)
import Lorentz.Contracts.SomeContractStorage
import Lorentz.Contracts.SomeContractParam
import Michelson.Typed.Sing.Missing

---------------
-- Read Address
---------------

-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftM2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $
        ensureAddressPrefix >>
        P.munch1 isAlphaNum >>= \addressStr ->
          case Tezos.parseAddress $ T.pack addressStr of
            Left err -> fail $ show err
            Right address' -> return address'
  where
    ensureAddressPrefix =
      (do {('t':'z':'1':_) <- P.look; return ()}) <|>
      (do {('K':'T':'1':_) <- P.look; return ()})

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

-- END Read Address


-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " <> name <> "."
    ]

-- | Parse an address which can be suffixed with entrypoint name
-- (e.g. "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%entrypoint").
--
-- See `EntryPoints.parseEpAddress`
instance Read EpAddress where
  readPrec = do
    str <- readPrec
    case EntryPoints.parseEpAddress $ fromString str of
      Left err -> fail $ show err
      Right result' -> return result'

-- | See `EntryPoints.parseEpAddress`
parseEpAddress :: String -> Opt.Parser EpAddress
parseEpAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "EPADDRESS"
    , Opt.help $ "Address (which can be suffixed with an entrypoint name) of the " <> name <> "."
    ]

-- | Parse a `View` by parsing its arguments and @"callback-contract"@ address
parseView :: forall a r. NiceParameterFull r => Opt.Parser a -> Opt.Parser (View a (GetDefaultEntryPointArg r))
parseView parseArg =
  View <$> parseArg <*> fmap (callingDefTAddress . TAddress @r) (parseAddress "callback-contract")

-- | Parse a `View_`
parseView_ :: forall r. NiceParameterFull r => Proxy r -> Opt.Parser (View () (GetDefaultEntryPointArg r))
parseView_ _ = parseView @() @r $ pure ()


-- | Parse a `Bool` (optional) argument, given its field name
parseBool :: String -> Opt.Parser Bool
parseBool name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "BOOL"
    , Opt.help $
      "Bool representing whether the contract is initially " <> name <> "."
    ]

-- | Parse a `String`
parseString :: String -> Opt.Parser String
parseString name = Opt.strOption $ mconcat
  [ Opt.long name
  , Opt.metavar "STRING"
  , Opt.help $ "String representing the contract's initial " <> name <> "."
  ]


-- | Parse a natural number argument, given its field name
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " <> name <> "."
    ]


-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

-- | Parse whether to output on one line
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse a natural number argument, given its field name
parseAuto :: Read a => String -> String -> Opt.Parser a
parseAuto name description =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar $ toUpper <$> name
    , Opt.help description
    ]

readCrypto :: (T.Text -> Either CryptoParseError a) -> ReadPrec a
readCrypto f = do
  str <- readPrec
  case f $ fromString str of
    Left err -> fail $ "readCrypto: " <> show err
    Right result' -> return result'

instance Read Ed25519.PublicKey where
  readPrec = readCrypto Ed25519.parsePublicKey

instance Read Secp256k1.PublicKey where
  readPrec = readCrypto Secp256k1.parsePublicKey

instance Read P256.PublicKey where
  readPrec = readCrypto P256.parsePublicKey

instance Read PublicKey where
  readPrec =
    fmap PublicKeyEd25519 readPrec <|>
    fmap PublicKeySecp256k1 readPrec <|>
    fmap PublicKeyP256 readPrec

-- | Parse the signer keys
parseSignerKeys :: String -> Opt.Parser [PublicKey]
parseSignerKeys name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "List PublicKey"
    , Opt.help $ "Public keys of multisig " <> name <> "."
    ]

-- | Parse `SomeContractStorage`, see `parseSomeContractParam`
parseSomeContractStorage :: String -> Opt.Parser SomeContractStorage
parseSomeContractStorage name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractStorage param -- param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @T.Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " <> name
      ])

instance Read Ed25519.Signature where
  readPrec = readCrypto Ed25519.parseSignature

instance Read Secp256k1.Signature where
  readPrec = readCrypto Secp256k1.parseSignature

instance Read P256.Signature where
  readPrec = readCrypto P256.parseSignature


instance Read Signature where
  readPrec =
   fmap SignatureEd25519 readPrec <|>
   fmap SignatureSecp256k1 readPrec <|>
   fmap SignatureP256 readPrec <|>
   fmap SignatureGeneric readPrec

parseSignatures :: String -> Opt.Parser (Maybe [Maybe Signature])
parseSignatures name =
  Opt.option Opt.auto $ mconcat
    [ Opt.long name
    , Opt.metavar "Maybe [Maybe Signature]"
    , Opt.help "Ordered list of signatures, with Nothing for missing. Elide to dump the message to sign."
    ]

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
  Opt.strOption @T.Text
    (mconcat
      [ Opt.long $ name <> "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " <> name
      ])

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @T.Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " <> name
      ])

parseSomeContract :: String -> Opt.Parser TypeCheck.SomeContract
parseSomeContract name =
  Opt.option (Opt.str >>= someContractParser)
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Contract Source"
      , Opt.help $ "The Michelson contract: " <> name
      ])
  where
  someContractParser :: T.Text -> Opt.ReadM TypeCheck.SomeContract
  someContractParser = either (fail . show) (either (fail . show) return . typeCheckContract mempty . expandContract) . parse program name

