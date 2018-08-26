#!/usr/bin/env stack
-- stack --resolver lts-12.0 script
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Network.Wai.Handler.Launch
import Yesod.Core
import RIO hiding (Handler (..))
import RIO.FilePath (takeDirectory)
import RIO.Process
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import Conduit
import Yesod.EventSource
import Network.Wai.EventSource
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Aeson (toEncoding, fromEncoding, Value (String))
import Text.Hamlet (hamletFile)
import Text.Lucius (Css, luciusFile)
import Text.Julius (Javascript, juliusFile)

data Result = Result !Word !Text

data App = App
  { appCode :: !(TMVar ByteString)
  , appResult :: !(TVar Result)
  }

mkYesod "App" [parseRoutes|
/ HomeR GET
/static/style.css StyleR GET
/static/script.js ScriptR GET
/submit SubmitR PUT
/result ResultR GET
|]

instance Yesod App where
  makeSessionBackend _ = pure Nothing

title :: Text
title = "Kids Coding Haskell IDE"

getHomeR :: Handler Html
getHomeR =
  $(hamletFile "template/home.hamlet") <$> getUrlRenderParams

getStyleR :: Handler Css
getStyleR =
  $(luciusFile "template/home.lucius") <$> getUrlRenderParams

getScriptR :: Handler Javascript
getScriptR = do
  $(juliusFile "template/home.julius") <$> getUrlRenderParams

putSubmitR :: Handler ()
putSubmitR = do
  bs <- fmap mconcat $ runConduit $ rawRequestBody .| sinkList
  app <- getYesod
  atomically $ do
    void $ tryTakeTMVar $ appCode app
    putTMVar (appCode app) bs

getResultR :: Handler TypedContent
getResultR = do
  app <- getYesod
  ioToRepEventSource Nothing $ \_ mlastCount -> do
    Result count text <- atomically $ do
      res@(Result count _) <- readTVar $ appResult app
      for_ mlastCount $ \lastCount -> checkSTM $ count /= lastCount
      pure res
    pure ([ServerEvent Nothing Nothing [fromEncoding $ toEncoding $ String text]], Just count)

builder :: App -> IO a
builder app = runSimpleApp $ withSystemTempFile "kids-coding.hs" $ \fp h -> do
  hClose h
  forever $ do
    code <- atomically $ takeTMVar $ appCode app
    writeFileBinary fp code
    (ec, out, err) <- withWorkingDir (takeDirectory fp) $ proc "stack" ["--resolver", "ghc-8.4.3", "runghc", fp] readProcess
    let text = utf8BuilderToText $
          (if ec == ExitSuccess then mempty else "It didn't work, sorry :(\n") <>
          display (decodeUtf8With lenientDecode (BL.toStrict out)) <>
          (if BL.null err then mempty else "\nError messages:\n" <> display (decodeUtf8With lenientDecode (BL.toStrict err)))
    atomically $ do
      Result count _ <- readTVar $ appResult app
      writeTVar (appResult app) $! Result (count + 1) text

main :: IO ()
main = do
  code <- newEmptyTMVarIO
  result <- newTVarIO $ Result 0 "Haven't run any code yet, get started!"
  let app = App
        { appCode = code
        , appResult = result
        }

  race_ (builder app) $ toWaiApp app >>= run
