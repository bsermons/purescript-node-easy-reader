module Node.ReadLine.Easy
       ( module RLA
       , module RLC
       , read_
       ) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Reader (class MonadAsk)
import Data.Newtype (class Newtype, wrap)
import Node.ReadLine (Interface, READLINE)
import Node.ReadLine.Aff as RLA
import Node.ReadLine.Class (class Readable, read)
import Node.ReadLine.Class as RLC


read_
  :: forall t a m eff
   . Newtype t a
  => MonadAsk Interface m
  => MonadAff (readline:: READLINE, console :: CONSOLE | eff) m
  => Readable a m
  => String
  -> m t
read_ q = do
  (r :: a) <- read q
  pure $ wrap r

