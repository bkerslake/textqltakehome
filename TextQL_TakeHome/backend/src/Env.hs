{-# LANGUAGE OverloadedStrings #-}
module Env where

import           Control.Monad.Reader
import           Data.Aeson                 (Value (..), Object, decode)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Vector                as V
import           Database.SQLite.Simple

data Env = Env
  { envDbConn :: Connection
  , envData   :: [Object]   -- ^ in-memory copy of seed JSON
  }

type AppM = ReaderT Env IO

initEnv :: FilePath -> FilePath -> IO Env
initEnv dbPath jsonPath = do
  conn <- open dbPath
  initializeDb conn
  raw  <- BSL.readFile jsonPath
  case decode raw of
    Just (Array arr) ->
      pure $ Env conn [o | Object o <- V.toList arr]
    _ -> error "seed JSON must be an array of flat objects"

initializeDb :: Connection -> IO ()
initializeDb c =
  execute_ c "CREATE TABLE IF NOT EXISTS query_history \
             \(id INTEGER PRIMARY KEY AUTOINCREMENT,\
             \ query TEXT, result TEXT, \
             \ timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)"
