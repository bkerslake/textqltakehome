{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Reader        (ask, runReaderT)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (object, (.=), encode, Value (..))
import           Data.Text                   (Text)
import           Data.Aeson                  (object, (.=))
import           Env
import           MyQuery
import qualified Data.Text                   as TS
import qualified Data.Text.Encoding          as TE
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import qualified Data.ByteString.Lazy        as BL
import           Network.Wai.Middleware.Cors
import           System.Environment          (getArgs)
import           Text.Megaparsec             (parse, errorBundlePretty)
import           Web.Scotty.Trans
import           Database.SQLite.Simple      (execute, query_)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFile] -> do
      env <- initEnv "db.db" jsonFile
      scottyT 3000 (`runReaderT` env) $ do
        middleware $ cors $ const $ Just simpleCorsResourcePolicy
          { corsMethods        = ["GET", "POST"]
          , corsRequestHeaders = ["Content-Type"] }

        ------------------------------------------------------------------
        --  GET /history  ------------------------------------------------
        ------------------------------------------------------------------
        get "/history" $ do
          e <- lift ask
          rows <- liftIO $
            (query_
               (envDbConn e)
               "SELECT query, result, timestamp \
               \FROM query_history ORDER BY timestamp DESC LIMIT 10"
             :: IO [(Text, Text, Text)])

          let rowsJson =
                [ object [ "query"     .= q
                         , "result"    .= r
                         , "timestamp" .= t ]
                | (q, r, t) <- rows
                ]
          json rowsJson

        ------------------------------------------------------------------
        --  POST /query  -------------------------------------------------
        ------------------------------------------------------------------
        post "/query" $ do
          incoming <- jsonData        -- TL.Text
          let sqlStrict :: Text
              sqlStrict = TL.toStrict incoming

          env' <- lift ask
          case parse queryP "" (TL.unpack incoming) of
            ----------------------------------------------------------------
            -- parse error --------------------------------------------------
            ----------------------------------------------------------------
            Left perr -> do
              let msg = TS.pack (errorBundlePretty perr)
              liftIO $ execute (envDbConn env')
                       "INSERT INTO query_history (query, result) VALUES (?,?)"
                       (sqlStrict, msg)
              json $ object ["error" .= msg]

            ----------------------------------------------------------------
            -- success ------------------------------------------------------
            ----------------------------------------------------------------
            Right qry -> do
              let filtered = maybe id (filter . evalCond) (qCond qry) (envData env')
                  limited  = maybe filtered (`take` filtered) (qLim qry)
                  result   = map (project (qFields qry)) limited

                  -- encode → ByteString → strict Text so SQLite stores TEXT
                  resText :: Text
                  resText = TE.decodeUtf8 (BL.toStrict (encode (map Object result)))

              liftIO $ execute (envDbConn env')
                       "INSERT INTO query_history (query, result) VALUES (?,?)"
                       (sqlStrict, resText)

              json $ object ["result" .= result]

    _ -> putStrLn "usage: textql-server <seed.json>"