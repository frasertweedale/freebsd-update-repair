{-# LANGUAGE OverloadedStrings #-}

module Repair
  (
    repair
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.Free
import Data.Char (toLower)
import Data.Configurator
import Data.Configurator.Types (Config)
import Data.Foldable (forM_)
import Data.List (isPrefixOf)
import System.IO
import System.Posix (createDirectory, createLink, removeLink, setFileMode)

import Index
import Inspect
import Object

repair :: Config -> Free Discrepancy r -> IO ()
repair conf (Free f) =
  let
    (eff, k) = go f
  in
    eff `catchError` print >> repair conf k
  where
  go (Missing a k)        = (repairMissing conf a, k)
  go (DigestDiffers a k)  = (repairDigest conf a, k)
  go (InodeDiffers a k)   = (repairInode conf a, k)
  go (ModeDiffers a k)    = (repairMode conf a, k)
  go (OwnerDiffers a k)   = (repairOwner conf a, k)
  go (GroupDiffers a k)   = (repairGroup conf a, k)
repair _ _ = return ()

autoRepairPrefixes :: Config -> IO [String]
autoRepairPrefixes conf = lookupDefault [] conf "autoRepairPrefixes"

repairMissing :: Config -> IndexEntry -> IO ()
repairMissing c a =
  isAuto c a >>= confirm ("Missing " ++ filePath a ++ ". Install?")
    (case fileType a of
      D -> createDirectory (filePath a) (mode a)
      F -> put a
      L -> putStrLn "not implemented: write symbolic links"
    )

repairDigest :: Config -> IndexEntry -> IO ()
repairDigest c a =
  isAuto c a >>= confirm ("Digest differs for " ++ filePath a ++ ". Repair?")
    (put a)

repairInode :: Config -> IndexEntry -> IO ()
repairInode c a =
  isAuto c a >>= confirm (filePath a ++ " needs to be hard linked. Repair?")
    (forM_ (link a) (\tgt -> removeLink lnk >> createLink tgt lnk))
  where
  lnk = filePath a

repairMode :: Config -> IndexEntry -> IO ()
repairMode c a =
  isAuto c a >>= confirm (filePath a ++ " has incorrect permission. Repair?")
    (setFileMode (filePath a) (mode a))

repairOwner :: Config -> IndexEntry -> IO ()
repairOwner _ _ = return ()

repairGroup :: Config -> IndexEntry -> IO ()
repairGroup _ _ = return ()

isAuto :: Config -> IndexEntry -> IO Bool
isAuto conf a = any (`isPrefixOf` filePath a) <$> autoRepairPrefixes conf

confirm :: String -> IO () -> Bool -> IO ()
confirm s m auto = if auto then m else do
  putStr (s ++ " [y/N]: ") >> hFlush stdout
  l <- getLine
  when ("y" `isPrefixOf` map toLower l) m

put :: IndexEntry -> IO ()
put a = putObject (target a) (filePath a)
