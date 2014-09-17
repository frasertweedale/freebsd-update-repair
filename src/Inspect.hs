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
import Data.Map (Map)
import Data.Semigroup.Monad
import System.Posix
import System.Process (readProcess)

import Index

data Discrepancy k
  = Missing IndexEntry k
  | DigestDiffers IndexEntry k
  -- | TypeDiffers FilePath FileType FileType k
  | ModeDiffers IndexEntry k
  | OwnerDiffers IndexEntry k
  | GroupDiffers IndexEntry k
  deriving (Show)

instance Functor Discrepancy where
  fmap f (Missing a k) = Missing a (f k)
  fmap f (DigestDiffers a k) = DigestDiffers a (f k)
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
checkStatus entry = when (isNothing (link entry)) $ do
  status <- lift $ getSymbolicLinkStatus (filePath entry)
  -- TODO check type
  unless (fileOwner status == uid entry) $ tell (ownerDiffers entry)
  unless (fileGroup status == gid entry) $ tell (groupDiffers entry)
  unless (fileMode status .&. 0o7777 `xor` mode entry == 0)
    $ tell (modeDiffers entry)

inspectEntry
  :: [FilePath]   -- ^ List of paths to ignore.
  -> IndexEntry
  -> WriterT (Action (Free Discrepancy)) IO ()
inspectEntry ignore entry =
  unless (any (`isPrefixOf` filePath entry) ignore) $ do
    exist <- lift $ fileExist (filePath entry)
    if exist
    then
      checkDigest entry
      >> checkStatus entry
    else
      tell $ missing entry

inspectIndex :: [FilePath] -> Map a IndexEntry -> IO (Free Discrepancy ())
inspectIndex ignore = fmap getAction . execWriterT . mapM (inspectEntry ignore)

inspect :: a -> Free Discrepancy r -> IO ()
inspect conf (Free (Missing a k)) =
  putStrLn ("MISS " ++ filePath a) >> inspect conf k
inspect conf (Free (ModeDiffers a k)) =
  putStrLn ("MODE " ++ filePath a) >> inspect conf k
inspect conf (Free (DigestDiffers a k)) =
  putStrLn ("HASH " ++ filePath a) >> inspect conf k
inspect conf (Free (OwnerDiffers a k)) =
  putStrLn (" UID " ++ filePath a) >> inspect conf k
inspect conf (Free (GroupDiffers a k)) =
  putStrLn (" GID " ++ filePath a) >> inspect conf k
inspect _ _ = return ()
