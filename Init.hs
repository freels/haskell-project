#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns #-}
module Main where
import System.Directory
import System.IO
import Control.Applicative
import Data.List
import Data.Char

main :: IO ()
main = do
  let fields = ["PROJECT NAME", "AUTHOR", "EMAIL", "PROJECT HOMEPAGE", "REPO"]
  dir       <- getCurrentDirectory
  let git    = join dir ".git"
  files     <- filter (not . startswith git) <$> findFiles dir
  subs      <- getTemplateInfo fields
  sequence_ (rewriteFile (flip replaceAll subs) <$> files)

  --where findAndReplace subs str =

findFiles :: FilePath -> IO [FilePath]
findFiles path = do
  isFile <- doesFileExist path

  if isFile then return [path]
  else do
    files <- filter notDot <$> getDirectoryContents path
    flattenM (findFiles <$> (join path) <$> files)

notDot f = f /= ".." && f /= "."

flattenM = fmap (>>= id) . sequence

join a b = a ++ "/" ++ b

startswith prefix (stripPrefix prefix -> Just _) = True
startswith _ _                                   = False

gsub from to (stripPrefix from -> Just rest) = to ++ (gsub from to rest)
gsub from to (c:rest)                        = c : (gsub from to rest)
gsub from to []                              = []

replaceAll = foldl replace
  where replace str (from, to) = gsub from to str

rewriteFile :: (String -> String) -> FilePath -> IO ()
rewriteFile f path = do
  contents <- readFile path
  putStrLn ("Writing file " ++ path)
  putStrLn (f contents)

getTemplateInfo fields = sequence $ map getSub fields
  where getSub field = (,) field <$> prompt (capitalize field)

prompt p = do
  putStr (p ++ ": ") >> (hFlush stdout)
  getLine >>= \x -> case x of
    []     -> putStrLn "Cannot be blank." >> prompt p
    answer -> return answer

capitalize (c:cs) = (toUpper c) : (map toLower cs)
capitalize []     = []
