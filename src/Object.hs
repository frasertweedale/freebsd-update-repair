module Object 
  (
    ObjectId
  , readObject
  , putObject
  ) where

import Control.Applicative

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as B

type ObjectId = String

readObject :: ObjectId -> IO B.ByteString
readObject oid =
  decompress <$> B.readFile ("/var/db/freebsd-update/files/" ++ oid ++ ".gz")

putObject :: ObjectId -> FilePath -> IO ()
putObject oid path = readObject oid >>= B.writeFile path
