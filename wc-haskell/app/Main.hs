{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Control.Concurrent
import qualified Data.ByteString.Lazy as B
import Data.List
import Control.Monad.IO.Class
import qualified Control.Monad.Par.IO  as ParIO
import qualified Control.Monad.Par  as Par
import qualified Control.Parallel.Strategies as PS
import qualified Control.Concurrent.Async as CCA
import GHC.Generics (Generic)
import qualified System.IO.MMap as MMap
import qualified Control.Concurrent.ParallelIO.Global as Parallel
import System.Directory
  ( getCurrentDirectory
  )
import System.Environment

import Lib

data LineCount = LineCount FilePath Int deriving (Eq, Generic, Par.NFData)

instance Show LineCount where
  show (LineCount path count) = (leftPad 10 $ show count) ++ " " ++ path

instance Ord LineCount where
  (LineCount _ c1) `compare` (LineCount _ c2) = c1 `compare` c2

leftPad :: Int -> String -> String
leftPad n s =
  let spaces = take (n - (length s)) $ repeat ' '
  in spaces ++ s

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

printLineCounts :: [LineCount] -> IO ()
printLineCounts lcs =
  mapM_ (putStrLn . show) (reverse $ sort lcs)

printTotal :: Int -> IO ()
printTotal total =
  putStrLn $ (leftPad 10 $ show total) ++ " " ++ "[TOTAL]"

-- countLinesTask :: FilePath -> (FilePath, B.ByteString) -> LineCount
-- countLinesTask currentDir (path, lines) = do
--   let path' = normalizePathToLocal path
--   LineCount path' (countLines lines)
--   where normalizePathToLocal = \path ->
--                                  case stripPrefix (currentDir ++ "/") path of
--                                    Just stripped -> stripped
--                                    Nothing -> path

countLinesTask :: FilePath -> FilePath -> IO LineCount
countLinesTask currentDir path = do
  lines <- B.readFile path
  let path' = normalizePathToLocal path
  return $ LineCount path' (countLines lines)
  where normalizePathToLocal = \path ->
                                 case stripPrefix (currentDir ++ "/") path of
                                   Just stripped -> stripped
                                   Nothing -> path

main :: IO ()
main = do
  args <- getArgs
  currentDir <- getCurrentDirectory
  let dir = case head' args of
              Just path -> path
              Nothing -> currentDir
  files <- getFilesInDir dir
--  contents <- mapM (\x -> MMap.mmapFileByteStringLazy x Nothing) files
  lineCounts <- Parallel.parallel $ map (countLinesTask currentDir) files
  printLineCounts lineCounts
  let total = foldr (\(LineCount _ count) t -> count + t) 0 lineCounts
  printTotal total
