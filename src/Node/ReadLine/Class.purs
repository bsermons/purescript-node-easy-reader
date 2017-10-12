module Node.ReadLine.Class where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Maybe (Maybe)
import Data.Record.Builder as Builder
import Node.ReadLine (Interface, READLINE)
import Node.ReadLine.Aff as RLA
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(..), SProxy(..), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)


class Readable m a where
  read :: String -> m a

instance readableString
  :: ( MonadAff (readline :: READLINE | e) m
     , MonadAsk Interface m
     ) => Readable m String where
  read q = do
    i <- ask
    liftAff $ RLA.readString q i

instance readableMaybeInt
  :: ( MonadAff (readline :: READLINE | e) m
     , MonadAsk Interface m
     ) => Readable m (Maybe Int) where
  read q = do
    i <- ask
    liftAff $ RLA.readMaybeInt q i

instance readableInt
  :: ( MonadAff (readline :: READLINE | e) m
     , MonadAsk Interface m)
  => Readable m Int where
  read q = do
    i <- ask
    liftAff $ RLA.readInt q i

instance readableRecord
  :: ( MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , MonadAsk Interface m
     , RowToList r lst
     , ReadableFields m lst () r
     ) => Readable m (Record r) where
  read q = do
    liftAff $ liftEff $ Console.log q
    i <- ask
    steps <- readFields (RLProxy :: RLProxy lst)
    pure $ Builder.build steps {}


class Monad m <= ReadableFields m (rl :: RowList) (from :: #Type) (to :: #Type) | rl -> from to where
  readFields :: RLProxy rl -> m (Builder.Builder (Record from)(Record to))

instance readableFieldsCons
  :: ( IsSymbol name
     , Readable m ty
     , ReadableFields m tail from from'
     , MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , RowLacks name from'
     , RowCons name ty from' to
     ) => ReadableFields m (Cons name ty tail) from to where
  readFields _ = do
    row :: ty  <- read (reflectSymbol name)
    rest <- readFields (RLProxy :: RLProxy tail)
    pure $ (Builder.insert name row) <<< rest

    where
      name = SProxy :: SProxy name

instance readableFieldsNil
  :: ( MonadAff (console :: CONSOLE, readline :: READLINE | e) m
     , MonadAsk Interface m
     ) => ReadableFields m Nil () () where
    readFields _ = pure id



readRecord
  :: forall r lst eff
   . RowToList r lst
  => ReadableFields (ReaderT Interface (Aff (readline :: READLINE, console :: CONSOLE | eff))) lst () r
  => Interface
  -> String
  -> Aff (readline :: READLINE, console :: CONSOLE | eff) (Record r)
readRecord i q = do
  liftAff $ liftEff $ Console.log q
  flip runReaderT i $ do
    steps <- readFields (RLProxy :: RLProxy lst)
    pure $ Builder.build steps {}
