#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Applicative
import System.Directory
import System.IO
import Data.List
import Data.Char

fields   = ["PNAME", "PAUTHOR", "PEMAIL", "PHOMEPAGE", "PREPO"]
excludes = [".git", "Init.hs"]

main :: IO ()
main = do
  dir             <- getCurrentDirectory
  let excludePaths = map (joinPath dir) excludes
  subs            <- getTemplateInfo fields
  files           <- filter (not . startsWithAny excludePaths) <$> findFiles dir
  sequence_ (rewriteFile (replaceAll subs) <$> files)
  let projName     = snd . head $ subs
      cabalFile    = joinPath dir (projName ++ ".cabal")
  renameFile (joinPath dir "project.cabal") (cabalFile)
  removeFile $ joinPath dir "Init.hs"

  where
    startsWithAny ps list  = any (startsWith $ list) ps
    startsWith list prefix = take (length prefix) list == prefix

joinPath a b = a ++ "/" ++ b

findFiles :: FilePath -> IO [FilePath]
findFiles path = do
  doesFileExist path >>= \x -> case x of
    True  -> return [path]
    False -> do
      files       <- getDirectoryContents path
      let filtered = filter (\x -> x /= ".." && x /= ".") files
      concat <$> (sequence $ findFiles <$> (joinPath path) <$> filtered)

gsub from to (stripPrefix from -> Just rest) = to ++ (gsub from to rest)
gsub from to (c:rest)                        = c : (gsub from to rest)
gsub from to []                              = []

replaceAll = flip $ foldl gsub'
  where gsub' str (from, to) = gsub from to str

rewriteFile :: (String -> String) -> FilePath -> IO ()
rewriteFile f path = do
  -- force contents thunk by getting its length.
  contents <- readFile path >>= \s -> length s `seq` return s
  putStrLn ("Writing file " ++ path)
  writeFile path (f contents)

getTemplateInfo fields = sequence $ map getSub fields
  where
    getSub field = (,) field <$> prompt (capitalize $ gsub "$" "" field)
    prompt p = do
      putStr (p ++ ": ") >> (hFlush stdout)
      getLine >>= \x -> case x of
        []     -> putStrLn "Cannot be blank." >> prompt p
        answer -> return answer

capitalize (c:cs) = (toUpper c) : (map toLower cs)
capitalize []     = []
