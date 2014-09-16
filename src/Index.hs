{-# LANGUAGE OverloadedStrings #-}

module Index where

import Control.Applicative
import Data.Char (ord)
import Numeric (readOct, showOct)

import Control.Monad.Except
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as B
import Data.Csv hiding (lookup)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Vector (toList)
import System.Posix

import Object

type Component = String
type SubComponent = String


data FileType = D | F | L
  deriving (Eq, Show)

instance FromField FileType where
  parseField "d" = pure D
  parseField "f" = pure F
  parseField "L" = pure L
  parseField _   = empty


newtype OctMode = OctMode { getMode :: FileMode }
  deriving (Eq)

instance Show OctMode where
  show (OctMode x) = "FileMode " ++ showOct x ""

instance FromField OctMode where
  parseField s = case readOct $ T.unpack $ T.decodeUtf8 s of
    (n, ""):_ -> pure $ OctMode n
    _         -> empty


data IndexEntry = IndexEntry
  { component :: Component
  , subComponent :: SubComponent
  , filePath :: FilePath
  , fileType :: FileType
  , uid :: CUid
  , gid :: CGid
  , mode :: CMode
  , flags :: Int
  , target :: String -- TODO Either Digest FilePath
  , link :: Maybe String  -- hardlink
  }
  deriving (Eq, Show)


instance FromRecord IndexEntry where
  parseRecord v = IndexEntry
    <$> v .! 0
    <*> v .! 1
    <*> v .! 2
    <*> v .! 3
    <*> fmap fromIntegral (v .! 4 :: Parser Int)
    <*> fmap fromIntegral (v .! 5 :: Parser Int)
    <*> fmap getMode (v .! 6)
    <*> v .! 7
    <*> v .! 8
    <*> v .! 9


data Error
  = TIndexError String
  | IndexError String
  deriving (Show)

readTIndex :: ExceptT Error IO [(String,String)]
readTIndex = ExceptT
  $ bimap TIndexError toList . decodeWith decodeOptions NoHeader
    <$> B.readFile "/var/db/freebsd-update/tINDEX.present"

readIndex :: String -> ExceptT Error IO (M.Map FilePath IndexEntry)
readIndex k = do
  tIndex <- readTIndex
  indexData <- ExceptT
    $ maybe (return (Left $ IndexError k)) (fmap Right . readObject)
    $ lookup k tIndex
  ExceptT $
    return $ bimap IndexError toMap $ decodeWith decodeOptions NoHeader indexData
  where
  toMap = M.fromList . map (\entry -> (filePath entry, entry)) . toList

decodeOptions :: DecodeOptions
decodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord '|') }
