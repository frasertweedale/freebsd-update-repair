module Inspect where

import Control.Applicative
import Data.Char (isSpace)
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
  | ModeDiffers IndexEntry k
  | DigestDiffers IndexEntry k
  -- | TypeDiffers FilePath FileType FileType k
  -- | OwnerDiffers FilePath Int Int k
  -- | GroupDiffers FilePath Int Int k
  deriving (Show)

instance Functor Discrepancy where
  fmap f (Missing a k) = Missing a (f k)
  fmap f (ModeDiffers a k) = ModeDiffers a (f k)
  fmap f (DigestDiffers a k) = DigestDiffers a (f k)

missing :: IndexEntry -> Action (Free Discrepancy)
missing a = Action $ Free (Missing a (return ()))

modeDiffers :: IndexEntry -> Action (Free Discrepancy)
modeDiffers a = Action $ Free (ModeDiffers a (return ()))

digestDiffers :: IndexEntry -> Action (Free Discrepancy)
digestDiffers a = Action $ Free (DigestDiffers a (return ()))

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

inspectEntry :: IndexEntry -> WriterT (Action (Free Discrepancy)) IO ()
inspectEntry entry = do
  exist <- lift $ fileExist (filePath entry)
  if exist
  then
    checkDigest entry
  else
    tell $ missing entry

inspectIndex :: Map a IndexEntry -> IO (Free Discrepancy ())
inspectIndex = fmap getAction . execWriterT . mapM inspectEntry

inspect :: Free Discrepancy r -> String
inspect (Free (Missing a k))
  = "MISS " ++ filePath a ++ "\n" ++ inspect k
inspect (Free (ModeDiffers a k))
  = "MODE " ++ filePath a ++ "\n" ++ inspect k
inspect (Free (DigestDiffers a k))
  = "HASH " ++ filePath a ++ "\n" ++ inspect k
inspect _ = ""
