{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mempty)

import Control.Monad.Except
import Control.Monad.Free (Free)
import Data.Configurator
import Data.Configurator.Types (Config)
import qualified Data.Map as M
import Options.Applicative

import Index
import Inspect
import Repair

commands :: Parser (IO ())
commands = subparser
  (   command "inspect" (info (pure $ configure >>= run inspect) mempty)
  <>  command "repair" (info (pure $ configure >>= run repair) mempty)
  )
  where
  configure = load [Required "freebsd-update-repair.conf"]

run :: (Config -> Free Discrepancy () -> IO ()) -> Config -> IO ()
run interpreter conf = do
  ignorePrefixes <- lookupDefault [] conf "ignorePrefixes"
  index <-
    runExceptT (liftM2 M.union (readIndex "INDEX-NEW") (readIndex "INDEX-ALL"))
  diff <- inspectIndex ignorePrefixes $ either (error . show) id index
  interpreter conf diff

main :: IO ()
main = join $ execParser (info commands mempty)
