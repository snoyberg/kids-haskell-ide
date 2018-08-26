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

data Result = Result !Word !Text

data App = App
  { appCode :: !(TMVar ByteString)
  , appResult :: !(TVar Result)
  }

mkYesod "App" [parseRoutes|
/ HomeR GET
/submit SubmitR PUT
/result ResultR GET
|]

instance Yesod App

title :: Text
title = "Kids Coding Haskell IDE"

getHomeR :: Handler Html
getHomeR = do
  render <- getUrlRenderParams
  pure $ ($ render) [hamlet|
$doctype 5
<html lang=en>
  <head>
    <meta charset=utf-8>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>#{title}

    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2/codemirror.min.css" crossorigin=anonymous>

    <style>
      #editor-wrapper {
        border: 1px solid black;
        padding: 0.5em;
        margin: 0.5em;
      }

      #result {
        margin: 0.5em;
        padding: 0.5em;
        background: #f8f8f8;
      }

  <body>

    <div .container>
      <div .row>
        <h1>#{title}

      <div .row>
        <div .col-sm-6>
          <div #editor-wrapper>
            <textarea #editor>main = print "Hello World!"
        <div .col-sm-6>
          <pre #result>No results yet

    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2/codemirror.min.js" crossorigin=anonymous>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2/mode/haskell/haskell.js" crossorigin=anonymous>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous">

    <script>
      var cm = CodeMirror.fromTextArea(document.getElementById("editor"),
            { autofocus: true
            , mode: "haskell"
            }),
          submit = function() {
                     fetch("@{SubmitR}", {method: "PUT", body: cm.getValue()})
                   },
          result = document.getElementById("result")
          ;
      cm.on('change', submit);
      submit();

      new EventSource("@{ResultR}").onmessage = function(e) {
        result.innerText = JSON.parse(e.data);
      };
|]

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