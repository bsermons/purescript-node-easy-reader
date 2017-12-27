module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Reader (class MonadAsk, ReaderT(..), runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Node.ReadLine (Interface, READLINE, close, createConsoleInterface, noCompletion)
import Node.ReadLine.Class (class Readable)
import Node.ReadLine.Easy (class Readable, read)
import Node.ReadLine.Easy as RL

newtype ReadableM eff a = ReadableM (ReaderT Interface (Aff (console :: CONSOLE, readline :: READLINE | eff)) a)
-- newtype ReadableM eff a = ReadableM (ReaderT Interface (Aff eff) a)
derive instance newtypeReadableM :: Newtype (ReadableM eff a) _
derive newtype instance functorReadableM :: Functor (ReadableM eff)
derive newtype instance applyReadableM :: Apply (ReadableM eff)
derive newtype instance applicativeReadableM :: Applicative (ReadableM eff)
derive newtype instance bindReadableM :: Bind (ReadableM eff)
derive newtype instance monadReadableM :: Monad (ReadableM eff)
derive newtype instance monadReadableMAsk :: MonadAsk Interface (ReadableM eff)

-- instance readableReadableM
--   :: Readable a (ReadableM eff) where
--   read q = do
--     a <- RL.read q
--     pure $ a

type User =
  { name :: String
  , age :: Int
  , location :: { city :: String }
  }

newtype Bar = Bar { c :: Maybe Int }
newtype Foo = Foo { a :: String, b :: Bar }
newtype Baz = Baz String

derive instance newtypeBar :: Newtype Bar _
derive instance newtypeFoo :: Newtype Foo _
derive instance newtypeBaz :: Newtype Baz _
derive newtype instance readableBar :: Readable Bar (ReadableM eff)

readFoo :: forall eff. ReadableM eff Foo
readFoo = RL.read "What is foo?"

readBar :: forall eff. ReadableM eff Bar
readBar = RL.read "What is bar?"

-- instance readableBar
--   :: ( MonadAff (console :: CONSOLE , readline :: READLINE | e) m
--      , MonadAsk Interface m
--      ) => Readable Bar m where
--   read q = do
--     a <- RL.read q
--     pure $ Bar a

-- instance readableFoo
--   :: ( MonadAff (console :: CONSOLE , readline :: READLINE | e) m
--      , MonadAsk Interface m
--      ) => Readable Foo m where
--   read q = do
--     a <- RL.read q
--     pure $ Foo a



logError :: forall a e. Either Error a -> Eff (console :: CONSOLE | e) Unit
logError (Left err) = log (message err)
logError _ = pure unit

main :: Eff (exception :: EXCEPTION, readline :: READLINE, console :: CONSOLE) Unit
main = do
  i <- createConsoleInterface noCompletion

  runAff_ (\r -> logError r *> close i) $ do
    flip runReaderT i do
      user <- (RL.read "User Info:")
      liftEff $ log ("Hey " <> (unwrap (Foo user)).a)
