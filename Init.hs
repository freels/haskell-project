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
  let fields = ["PROJECT NAME", "AUTHOR", "EMAIL", "PROJECT HOMEPAGE", "REPO"]
  dir       <- getCurrentDirectory
  files     <- filter (not . startswith (joinPath dir ".git")) <$> findFiles dir
  subs      <- getTemplateInfo fields
  sequence_ (rewriteFile (flip replaceAll subs) <$> files)
  removeFile (joinPath dir "Init.hs")

findFiles :: FilePath -> IO [FilePath]
findFiles path = do
  doesFileExist path >>= \x -> case x of
    True  -> return [path]
    False -> do
      files <- filter notDot <$> getDirectoryContents path
      flattenM (findFiles <$> (joinPath path) <$> files)

notDot f = f /= ".." && f /= "."

flattenM = fmap (>>= id) . sequence

joinPath a b = a ++ "/" ++ b

startswith prefix list = take (length prefix) list == prefix

gsub from to (stripPrefix from -> Just rest) = to ++ (gsub from to rest)
gsub from to (c:rest)                        = c : (gsub from to rest)
gsub from to []                              = []

replaceAll = foldl replace
  where replace str (from, to) = gsub from to str

rewriteFile :: (String -> String) -> FilePath -> IO ()
rewriteFile f path = do
  contents <- readFile path
  putStrLn ("Writing file " ++ path)
  writeFile path (f contents)

getTemplateInfo fields = sequence $ map getSub fields
  where getSub field = (,) field <$> prompt (capitalize field)

prompt p = do
  putStr (p ++ ": ") >> (hFlush stdout)
  getLine >>= \x -> case x of
    []     -> putStrLn "Cannot be blank." >> prompt p
    answer -> return answer

capitalize (c:cs) = (toUpper c) : (map toLower cs)
capitalize []     = []
