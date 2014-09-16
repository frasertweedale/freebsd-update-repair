module Repair
  (
    repair
  , defaultRepairConfig
  ) where

import Control.Monad
import Control.Monad.Free
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.IO

import Index
import Inspect
import Object

data RepairConfig = RepairConfig
  { autoPutPrefixes :: [FilePath]
  }

defaultRepairConfig :: RepairConfig
defaultRepairConfig = RepairConfig
  { autoPutPrefixes =
    [ "/bin/", "/rescue/", "/usr/include/", "/usr/src/", "/usr/lib/"
    , "/usr/libexec/", "/usr/sbin/", "/usr/share/"
    ]
  }

repair :: RepairConfig -> Free Discrepancy r -> IO ()
repair conf (Free (Missing a k)) = repairMissing conf a >> repair conf k
repair conf (Free (DigestDiffers a k)) = repairDigest conf a >> repair conf k
repair conf (Free (ModeDiffers a k)) = repairMode conf a >> repair conf k
repair conf (Free (OwnerDiffers a k)) = repairOwner conf a >> repair conf k
repair conf (Free (GroupDiffers a k)) = repairGroup conf a >> repair conf k
repair _ _ = return ()

repairMissing :: RepairConfig -> IndexEntry -> IO ()
repairMissing conf a = if any (`isPrefixOf` filePath a) (autoPutPrefixes conf)
  then put a
  else confirm ("Missing " ++ filePath a ++ ". Install?") (put a)

repairDigest :: RepairConfig -> IndexEntry -> IO ()
repairDigest conf a = if any (`isPrefixOf` filePath a) (autoPutPrefixes conf)
  then put a
  else confirm ("Digest differs for " ++ filePath a ++ ". Repair?") (put a)

repairMode :: RepairConfig -> IndexEntry -> IO ()
repairMode _ _ = return ()

repairOwner :: RepairConfig -> IndexEntry -> IO ()
repairOwner _ _ = return ()

repairGroup :: RepairConfig -> IndexEntry -> IO ()
repairGroup _ _ = return ()

confirm :: String -> IO () -> IO ()
confirm s m = do
  putStr (s ++ " [y/N]:") >> hFlush stdout
  l <- getLine
  when ("y" `isPrefixOf` map toLower l) m

put :: IndexEntry -> IO ()
put a = putObject (target a) (filePath a)
