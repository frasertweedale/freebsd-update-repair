import Data.Monoid (mempty)

import Control.Monad.Except
import Control.Monad.Free (Free)
import qualified Data.Map as M
import Options.Applicative

import Index
import Inspect
import Repair

commands :: Parser (IO ())
commands = subparser
  (   command "inspect" (info (pure $ run inspect) mempty)
  <>  command "repair" (info (pure $ run (repair defaultRepairConfig)) mempty)
  )

run :: (Free Discrepancy () -> IO ()) -> IO ()
run interpreter =
  runExceptT (liftM2 M.union (readIndex "INDEX-NEW") (readIndex "INDEX-ALL"))
  >>= inspectIndex ignore . either (error . show) id
  >>= interpreter
  where
  ignore = ["/etc/", "/usr/share/doc/", "/var/db/"]

main :: IO ()
main = join $ execParser (info commands mempty)
