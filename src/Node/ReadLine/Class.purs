module Node.ReadLine.Class where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Record.Builder as Builder
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Node.ReadLine (Interface, READLINE)
import Node.ReadLine.Aff as RLA
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)


class Readable a m where
  read :: String -> m a

-- instance readableNewtype
--   :: ( MonadAff (readline :: READLINE, console :: CONSOLE | e) m
--      , MonadAsk Interface m
--      , Newtype t a
--      , Readable a m
--      ) => Readable t m where
--   read q = do
--     (a :: a) <- read q
--     pure $ wrap a

instance readableString
  :: ( MonadAff (readline :: READLINE | e) m
     , MonadAsk Interface m
     ) => Readable String m where
  read q = do
    i <- ask
    liftAff $ RLA.readString q i

instance readableMaybeInt
  :: ( MonadAff (readline :: READLINE | e) m
     , MonadAsk Interface m
     ) => Readable (Maybe Int) m where
  read q = do
    i <- ask
    liftAff $ RLA.readMaybeInt q i

instance readableInt
  :: ( MonadAff (readline :: READLINE | e) m
     , MonadAsk Interface m)
  => Readable Int m where
  read q = do
    i <- ask
    liftAff $ RLA.readInt q i

instance readableRecord
  :: ( MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , MonadAsk Interface m
     , RowToList r lst
     , ReadableFields lst () r m
     ) => Readable (Record r) m where
  read q = do
    liftAff $ liftEff $ Console.log q
    i <- ask
    steps <- readFields (RLProxy :: RLProxy lst)
    pure $ Builder.build steps {}


instance readableStrMap
  :: ( MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , MonadAsk Interface m
     , Readable a m
     ) => Readable (StrMap a) m where
  read q = do
    i <- ask
    readStrMap q i


class Monad m <= ReadableFields (rl :: RowList) (from :: #Type) (to :: #Type) m | rl -> from to where
  readFields :: RLProxy rl -> m (Builder.Builder (Record from)(Record to))

instance readableFieldsCons
  :: ( IsSymbol name
     , Readable ty m
     , ReadableFields tail from from' m
     , MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , RowLacks name from'
     , RowCons name ty from' to
     ) => ReadableFields (Cons name ty tail) from to m where
  readFields _ = do
    row :: ty  <- read (reflectSymbol name)
    rest <- readFields (RLProxy :: RLProxy tail)
    pure $ (Builder.insert name row) <<< rest

    where
      name = SProxy :: SProxy name

instance readableFieldsNil
  :: ( MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , MonadAsk Interface m
     ) => ReadableFields Nil () () m where
    readFields _ = pure id



readRecord
  :: forall r lst eff
   . RowToList r lst
  => ReadableFields lst () r (ReaderT Interface (Aff (readline :: READLINE, console :: CONSOLE | eff)))
  => Interface
  -> String
  -> Aff (readline :: READLINE, console :: CONSOLE | eff) (Record r)
readRecord i q = do
  liftAff $ liftEff $ Console.log q
  flip runReaderT i $ do
    steps <- readFields (RLProxy :: RLProxy lst)
    pure $ Builder.build steps {}


readStrMap
  :: forall m e a
   . Readable a m
  => MonadAff (readline :: READLINE | e) m
  => String
  -> Interface
  -> m (StrMap a)
readStrMap q i = flip runReaderT i $ do
  go StrMap.empty
  where
    go map = do
      key <- liftAff $ RLA.readString "key (type 'quit' to exit)" i
      if key == "quit"
        then pure map
        else do
          val <- lift $ read "value"
          go (StrMap.insert key val map)

