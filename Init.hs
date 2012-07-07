#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Applicative
import System.Directory
import System.IO
import Data.List
import Data.Char

main :: IO ()
main = do
  let fields   = ["PROJECT NAME", "AUTHOR", "EMAIL", "PROJECT HOMEPAGE", "REPO"]
  dir         <- getCurrentDirectory
  let excludes = map (joinPath dir) [".git", "Init.hs"]
  files       <- filter (not . startsWithAny excludes) <$> findFiles dir
  subs        <- getTemplateInfo fields
  sequence_ (rewriteFile (flip replaceAll subs) <$> files)

  where
    gsub' (from, to)       = gsub from to
    replaceAll             = foldl (flip gsub')
    startsWithAny ps list  = any (flip startsWith $ list) ps
    startsWith prefix list = take (length prefix) list == prefix

findFiles :: FilePath -> IO [FilePath]
findFiles path = do
  doesFileExist path >>= \x -> case x of
    True  -> return [path]
    False -> do
      files <- filter notDot <$> getDirectoryContents path
      flattenM (findFiles <$> (joinPath path) <$> files)

  where
    notDot f = f /= ".." && f /= "."
    flattenM = fmap (>>= id) . sequence

joinPath a b = a ++ "/" ++ b

gsub :: String -> String -> String -> String
gsub from to (stripPrefix from -> Just rest) = to ++ (gsub from to rest)
gsub from to (c:rest)                        = c : (gsub from to rest)
gsub from to []                              = []

rewriteFile :: (String -> String) -> FilePath -> IO ()
rewriteFile f path = do
  contents <- readFile path
  putStrLn ("Writing file " ++ path)
  writeFile path (f contents)

  where
    readFile' path = readFile path >>= \s -> length s `seq` return s

getTemplateInfo fields = sequence $ map getSub fields
  where
    getSub field = (,) field <$> prompt (capitalize field)
    prompt p = do
      putStr (p ++ ": ") >> (hFlush stdout)
      getLine >>= \x -> case x of
        []     -> putStrLn "Cannot be blank." >> prompt p
        answer -> return answer

capitalize (c:cs) = (toUpper c) : (map toLower cs)
capitalize []     = []
