module Lib
  ( getFilesInDir
  , countLines
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.Directory
  ( getCurrentDirectory
  , listDirectory
  )
import System.FilePath.Posix
  ( joinPath
  )
import System.PosixCompat.Files
  ( getFileStatus
  , isRegularFile
  )

getFilesInDir :: Maybe FilePath -> IO [FilePath]
getFilesInDir mPath =
  let locateDir = case mPath of
                    Just path -> return path
                    Nothing -> getCurrentDirectory
  in locateDir >>= listDirectory' >>= onlyRegularFiles

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' path =
  fullyQualify <$> listDirectory path
  where fullyQualify = map (\file -> joinPath [path, file])

onlyRegularFiles :: [FilePath] -> IO [FilePath]
onlyRegularFiles files = do
  areFiles <- mapM isRegularFile' files
  return $ zip files areFiles
    |> filter (\(_, isFile) -> isFile)
    |> map (\(f, _) -> f)
  where (|>) x f = f x

isRegularFile' :: FilePath -> IO Bool
isRegularFile' path =
  isRegularFile <$> getFileStatus path

countLines :: FilePath -> IO Int
countLines path =
  C.count '\n' <$> B.readFile path
