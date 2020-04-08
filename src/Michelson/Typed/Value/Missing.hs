
module Michelson.Typed.Value.Missing where

import Control.Monad (Monad(..))
import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Eq
import Data.Either
import Data.Function (id)
import Data.Functor
import Prelude (FilePath, IO, Ord(..), print, putStrLn)
import Data.String (String)
import Data.Maybe
import Data.Typeable

import Lorentz hiding (checkSignature, get)
import Michelson.Parser
import Michelson.Typed.Annotation
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Michelson.Typed.Value
import Michelson.Typed.Instr
import Michelson.Typed.EntryPoints
import Util.IO
import qualified Michelson.Untyped.Type as U
import Tezos.Crypto (checkSignature)
import qualified Michelson.TypeCheck.Types as TypeCheck
import Michelson.Macro
import Michelson.TypeCheck.Instr

-- import qualified Options.Applicative as Opt
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy.IO as TL
-- import qualified Data.ByteString.Base16 as Base16
-- import Data.Constraint
-- import Data.Singletons
-- import Text.Megaparsec (parse)

instance IsoValue (Value' Instr t) where
  type ToT (Value' Instr t) = t
  toVal = id
  fromVal = id

instance IsoCValue (Value ('Tc ct)) where
  type ToCT (Value ('Tc ct)) = ct
  toCVal (VC xs) = xs
  fromCVal = VC

-- | No `Notes`
instance SingI t => HasTypeAnn (Value' Instr t) where
  getTypeAnn = starNotes

