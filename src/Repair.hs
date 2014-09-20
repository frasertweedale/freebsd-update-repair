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
import System.Posix (createLink, removeLink)

import Index
import Inspect
import Object

repair :: Config -> Free Discrepancy r -> IO ()
repair conf (Free (Missing a k)) = repairMissing conf a >> repair conf k
repair conf (Free (DigestDiffers a k))  = repairDigest conf a >> repair conf k
repair conf (Free (InodeDiffers a k))   = repairInode conf a >> repair conf k
repair conf (Free (ModeDiffers a k))    = repairMode conf a >> repair conf k
repair conf (Free (OwnerDiffers a k))   = repairOwner conf a >> repair conf k
repair conf (Free (GroupDiffers a k))   = repairGroup conf a >> repair conf k
repair _ _ = return ()

autoRepairPrefixes :: Config -> IO [String]
autoRepairPrefixes conf = lookupDefault [] conf "autoRepairPrefixes"

repairMissing :: Config -> IndexEntry -> IO ()
repairMissing conf a = do
  auto <- any (`isPrefixOf` filePath a) <$> autoRepairPrefixes conf
  if auto
    then put a
    else confirm ("Missing " ++ filePath a ++ ". Install?") (put a)

repairDigest :: Config -> IndexEntry -> IO ()
repairDigest conf a = do
  auto <- any (`isPrefixOf` filePath a) <$> autoRepairPrefixes conf
  if auto
    then put a
    else confirm ("Digest differs for " ++ filePath a ++ ". Repair?") (put a)

repairInode :: Config -> IndexEntry -> IO ()
repairInode conf a = do
  auto <- any (`isPrefixOf` filePath a) <$> autoRepairPrefixes conf
  if auto
    then m
    else confirm (filePath a ++ " needs to be hard linked. Repair?") m
  where
  m = forM_ (link a) (\tgt -> removeLink lnk >> createLink tgt lnk)
  lnk = filePath a

repairMode :: Config -> IndexEntry -> IO ()
repairMode _ _ = return ()

repairOwner :: Config -> IndexEntry -> IO ()
repairOwner _ _ = return ()

repairGroup :: Config -> IndexEntry -> IO ()
repairGroup _ _ = return ()

confirm :: String -> IO () -> IO ()
confirm s m = do
  putStr (s ++ " [y/N]: ") >> hFlush stdout
  l <- getLine
  when ("y" `isPrefixOf` map toLower l) m

put :: IndexEntry -> IO ()
put a = putObject (target a) (filePath a) `catchError` print
