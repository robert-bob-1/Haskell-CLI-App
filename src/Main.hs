module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Control.Monad.IO.Class (liftIO)
usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- declare database

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    (Error err) -> putStrLn "Failed to load DB"
    (Success db) -> do
      let entry = DB.findFirst (\e -> entryId e == getOptId getOpts) db
      case entry of
        Nothing -> putStrLn "Failed to find entry"
        Just entry -> putStrLn (entrySnippet entry)
        
  return ()

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    Error err -> putStrLn "Failed to load DB"
    Success db -> do
      db2<- DB.load
      let entries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) (getSuccess db2 DB.empty)
      case entries of
        [] -> do
          putStrLn "No entries found"
        [entry] -> do
          putStrLn "Entry found: "
          putStrLn . show . FmtEntry $ entry
        _ -> do
          putStrLn "These are the found entries: " 
          mapM_ (putStrLn . show . FmtEntry) entries

  return ()

-- -- | Handle the add command
-- handleAdd :: TestableMonadIO m => AddOptions -> m ()
-- handleAdd addOpts =do
--   snip <- readFile (addOptFilename addOpts)
--   db <- DB.load
--   case db of
--     Error err -> putStrLn "Failed to load DB"
--     Success db -> do
--       putStrLn "Loaded DB succesfully"

--   let
--     dbsuccess = getSuccess db DB.empty
--     dbresult = DB.insertWith (\id -> addOptsToEntry id snip addOpts) dbsuccess
--   DB.save dbresult  
--   return ()
-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts =do
  snip <- readFile (addOptFilename addOpts)
  db <- DB.load
  case db of
    Error err -> putStrLn "Failed to load DB"
    Success db -> do
      putStrLn "Loaded DB succesfully"

  let
    dbsuccess = getSuccess db DB.empty
    checkIfExists = DB.findFirst (\e -> entrySnippet e == snip) dbsuccess
  case checkIfExists of
    Just entry -> do
      putStrLn "Entry with this content already exists: "
      putStrLn . show . FmtEntry $ entry
      return()
    Nothing -> do
      putStrLn "Snippet does not exist, inserting"
  let 
    dbresult = DB.insertWith (\id -> addOptsToEntry id snip addOpts) dbsuccess
  DB.save dbresult  
  return ()

addOptsToEntry :: Int -> String -> AddOptions -> Entry
addOptsToEntry id snippet addOpts =
  Entry 
  { entryId = id
  , entrySnippet = snippet
  , entryFilename = addOptFilename addOpts
  , entryLanguage = addOptLanguage addOpts
  , entryDescription = addOptDescription addOpts
  , entryTags = addOptTags addOpts
  }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
