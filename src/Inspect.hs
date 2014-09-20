{-# LANGUAGE OverloadedStrings #-}

module Inspect
  (
    Discrepancy(..)

  , inspect
  , inspectIndex
  ) where

import Control.Applicative
import Data.Bits
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Data.Traversable (mapM)

import Prelude hiding (mapM)
import Control.Monad.Free
import Control.Monad.Writer hiding (mapM)
import Data.Configurator (lookupDefault)
import Data.Configurator.Types (Config)
import Data.Map (Map)
import Data.Semigroup.Monad
import System.Posix
import System.Process (readProcess)

import Index

data Discrepancy k
  = Missing IndexEntry k
  | DigestDiffers IndexEntry k
  -- | TypeDiffers FilePath FileType FileType k
  | InodeDiffers IndexEntry k
  | ModeDiffers IndexEntry k
  | OwnerDiffers IndexEntry k
  | GroupDiffers IndexEntry k
  deriving (Show)

instance Functor Discrepancy where
  fmap f (Missing a k) = Missing a (f k)
  fmap f (DigestDiffers a k) = DigestDiffers a (f k)
  fmap f (InodeDiffers a k) = InodeDiffers a (f k)
  fmap f (ModeDiffers a k) = ModeDiffers a (f k)
  fmap f (OwnerDiffers a k) = OwnerDiffers a (f k)
  fmap f (GroupDiffers a k) = GroupDiffers a (f k)

missing :: IndexEntry -> Action (Free Discrepancy)
missing a = Action $ Free (Missing a (return ()))

digestDiffers :: IndexEntry -> Action (Free Discrepancy)
digestDiffers a = Action $ Free (DigestDiffers a (return ()))

modeDiffers :: IndexEntry -> Action (Free Discrepancy)
modeDiffers a = Action $ Free (ModeDiffers a (return ()))

ownerDiffers :: IndexEntry -> Action (Free Discrepancy)
ownerDiffers a = Action $ Free (OwnerDiffers a (return ()))

groupDiffers :: IndexEntry -> Action (Free Discrepancy)
groupDiffers a = Action $ Free (GroupDiffers a (return ()))

inodeDiffers :: IndexEntry -> Action (Free Discrepancy)
inodeDiffers a = Action $ Free (InodeDiffers a (return ()))

-- | Check whether the digest matches.
--
-- This check is only performed for regular files.  Hardlinks are
-- ignored, except for the "principal" file.
--
checkDigest :: IndexEntry -> WriterT (Action (Free Discrepancy)) IO ()
checkDigest entry = when (fileType entry == F && isNothing (link entry)) $ do
  digest <- lift $ trimDigest <$> readProcess "sha256" ["-q", filePath entry] []
  unless (digest == target entry) $ tell (digestDiffers entry)
  where
  trimDigest = takeWhile (not . isSpace)

-- | Check mode, owner and group of file.
--
-- Hardlinks are ignored, except for the "principal" file.
--
checkStatus :: IndexEntry -> WriterT (Action (Free Discrepancy)) IO ()
checkStatus entry = do
  status <- lift $ getSymbolicLinkStatus (filePath entry)
  -- TODO check type
  case link entry of
    Nothing -> do
      unless (fileOwner status == uid entry) $ tell (ownerDiffers entry)
      unless (fileGroup status == gid entry) $ tell (groupDiffers entry)
      unless (fileMode status .&. 0o7777 `xor` mode entry == 0)
        $ tell (modeDiffers entry)
    Just targetFilePath -> do
      targetExists <- lift $ fileExist targetFilePath
      when targetExists $ do  -- ignore if target does not exist
        targetStatus <- lift $ getSymbolicLinkStatus targetFilePath
        unless (fileID status == fileID targetStatus) $ tell (inodeDiffers entry)


shouldIgnore :: Config -> IndexEntry -> IO Bool
shouldIgnore conf entry = do
  ignorePrefixes <- lookupDefault [] conf "ignorePrefixes"
  ignoreComponents <- lookupDefault [] conf "ignoreComponents"
  let fullComponent = component entry ++ "/" ++ subComponent entry
  return $
    any (`isPrefixOf` filePath entry) ignorePrefixes
    || component entry `elem` ignoreComponents
    || fullComponent `elem` ignoreComponents

inspectEntry
  :: Config
  -> IndexEntry
  -> WriterT (Action (Free Discrepancy)) IO ()
inspectEntry conf entry = do
  ignore <- lift $ shouldIgnore conf entry
  unless ignore $ do
    exist <- lift $ fileExist (filePath entry)
    if exist
    then
      checkDigest entry
      >> checkStatus entry
    else
      tell $ missing entry

inspectIndex :: Config -> Map a IndexEntry -> IO (Free Discrepancy ())
inspectIndex conf = fmap getAction . execWriterT . mapM (inspectEntry conf)

inspect :: a -> Free Discrepancy r -> IO ()
inspect conf (Free (Missing a k)) =
  putStrLn ("MISS " ++ filePath a) >> inspect conf k
inspect conf (Free (DigestDiffers a k)) =
  putStrLn ("HASH " ++ filePath a) >> inspect conf k
inspect conf (Free (InodeDiffers a k)) =
  putStrLn ("INOD " ++ filePath a) >> inspect conf k
inspect conf (Free (ModeDiffers a k)) =
  putStrLn ("MODE " ++ filePath a) >> inspect conf k
inspect conf (Free (OwnerDiffers a k)) =
  putStrLn (" UID " ++ filePath a) >> inspect conf k
inspect conf (Free (GroupDiffers a k)) =
  putStrLn (" GID " ++ filePath a) >> inspect conf k
inspect _ _ = return ()
