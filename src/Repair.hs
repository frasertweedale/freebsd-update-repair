module Repair where

import Control.Monad
import Control.Monad.Free
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.IO

import Index
import Inspect
import Object

repair :: Free Discrepancy r -> IO ()
repair (Free (Missing a k)) = return ()
repair (Free (ModeDiffers a k)) = return ()
repair (Free (DigestDiffers a k)) = repairDigest a >> repair k
repair _ = return ()

repairDigest :: IndexEntry -> IO ()
repairDigest a = confirm ("Digest differs for " ++ filePath a ++ ". Repair?") $
  putObject (target a) (filePath a)

confirm :: String -> IO () -> IO ()
confirm s m = do
  putStr (s ++ " [y/n]:") >> hFlush stdout
  l <- getLine
  when ("y" `isPrefixOf` map toLower l) m
