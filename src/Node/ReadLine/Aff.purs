module Node.ReadLine.Aff where

import Prelude

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff.Exception (Error, error)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Node.ReadLine (Interface, READLINE)
import Node.ReadLine as ReadLine

read'
  :: forall e a
   . String
  -> Interface
  -> (String -> Either Error a)
  -> Aff (readline :: READLINE | e) a
read' q i conv =
  makeAff $ \cb -> do
    ReadLine.question q (cb <<< conv) i
    pure nonCanceler

readString
  :: forall e
   . String
  -> Interface
  -> Aff (readline :: READLINE | e) String
readString i q = read' i q pure

readMaybeInt
  :: forall e
   . String
  -> Interface
  -> Aff (readline :: READLINE | e) (Maybe Int)
readMaybeInt i q = read' i q (pure <<< Int.fromString)

readInt
  :: forall e
   . String
  -> Interface
  -> Aff (readline :: READLINE | e) Int
readInt i q = read' i q conv
  where
    conv s =
      case Int.fromString s of
        Nothing -> Left (error ("Error parsing int from '"<> s <> "'"))
        Just val -> Right val
