module Test.Main where

import Prelude

import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Node.ReadLine (READLINE, close, createConsoleInterface, noCompletion)
import Node.ReadLine.Easy as RL

type User =
  { name :: String
  , age :: Int
  , location :: { city :: String }
  }


logError :: forall a e. Either Error a -> Eff (console :: CONSOLE | e) Unit
logError (Left err) = log (message err)
logError _ = pure unit

main :: Eff (exception :: EXCEPTION, readline :: READLINE, console :: CONSOLE) Unit
main = do
  i <- createConsoleInterface noCompletion

  runAff_ (\r -> logError r *> close i) $ do
    flip runReaderT i do
      (user :: User) <- RL.read "User Info:"
      liftEff $ log ("Hey " <> user.name)
