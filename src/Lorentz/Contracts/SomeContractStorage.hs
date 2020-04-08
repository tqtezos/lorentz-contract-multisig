{-# OPTIONS -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.SomeContractStorage where

import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id)
import Data.Functor
import Prelude (FilePath, IO, Ord(..))
import Data.String (IsString(..), String)
import Data.Maybe
import Data.Typeable
import Text.Read

import Lorentz hiding (get)
import Michelson.Parser
import Michelson.Typed.Annotation
import Michelson.Typed.Arith
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


-- | Some contract storage with `SingI` and `HasNoOp` constraints
data SomeContractStorage where
  SomeContractStorage :: (SingI a, HasNoOp a)
    => Value a
    -> SomeContractStorage

-- | Run `SomeContractStorage`
fromSomeContractStorage :: forall b. SomeContractStorage -> (forall a. (SingI a, HasNoOp a) => Value a -> b) -> b
fromSomeContractStorage (SomeContractStorage xs) f = f xs

