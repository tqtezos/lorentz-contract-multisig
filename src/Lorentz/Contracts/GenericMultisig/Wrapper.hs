{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.GenericMultisig.Wrapper where

import Data.Singletons
import Text.Megaparsec (eof)

import Lorentz
import Michelson.TypeCheck.TypeCheck
import Michelson.TypeCheck.Instr
import Michelson.Parser hiding (parseValue)
import Michelson.Macro

import Control.Applicative
import Text.Show
import Prelude ((<$>), (>>=), fail, either, flip, runReaderT)

-- | Parse and typecheck a Michelson value
parseTypeCheckValue ::
     forall t. (SingI t)
  => Parser (Value t)
parseTypeCheckValue =
  (>>= either (fail . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeCheckValue . expandValue <$>
  (value <* eof)

