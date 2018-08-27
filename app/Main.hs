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
import Options.Applicative.Simple
import qualified Paths_kids_ide

data Result = Result !Word !Text !Bool

data App = App
  { appCode :: !(TMVar ByteString)
  , appResult :: !(TVar Result)
  , appSnapshot :: !String
  }

setResult :: App -> Text -> Bool -> STM ()
setResult app text isSuccess = do
  Result count _ _ <- readTVar $ appResult app
  writeTVar (appResult app) $! Result (count + 1) text isSuccess

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
getHomeR = do
  app <- getYesod
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
    Result count text isSuccess <- atomically $ do
      res@(Result count _ _) <- readTVar $ appResult app
      for_ mlastCount $ \lastCount -> checkSTM $ count /= lastCount
      pure res
    let obj = object
          [ "text" .= text
          , "success" .= isSuccess
          ]
    pure ([ServerEvent Nothing Nothing [fromEncoding $ toEncoding obj]], Just count)

builder :: App -> IO a
builder app = runSimpleApp $ withSystemTempFile "kids-coding.hs" $ \fp h -> do
  hClose h
  forever $ do
    code <- atomically $ takeTMVar $ appCode app
    writeFileBinary fp code
    (ec, out, err) <- withWorkingDir (takeDirectory fp)
      $ proc
          "stack"
          ["--resolver", appSnapshot app, "runghc", fp]
          readProcess
    let text = utf8BuilderToText $
          (if ec == ExitSuccess then mempty else "It didn't work, sorry :(\n") <>
          display (decodeUtf8With lenientDecode (BL.toStrict out)) <>
          (if BL.null err then mempty else "\nError messages:\n" <> display (decodeUtf8With lenientDecode (BL.toStrict err)))
    atomically $ setResult app text (ec == ExitSuccess)

data Opts = Opts
  { optsPort :: !(Maybe Int)
  , optsSnapshot :: !String
  }

parseOpts :: IO (Opts, ())
parseOpts =
  simpleOptions
    $(simpleVersion Paths_kids_ide.version)
    "Kids IDE"
    "Provide a minimalistic environment for kids to learn programming with Haskell"
    parser
    empty
  where
    parser = Opts
      <$> optional
            (option auto
              (long "port" <> metavar "PORT" <> help "Set a port, disabling the auto-launch feature"))
      <*> strOption (long "snapshot" <> metavar "VERSION" <> help "GHC version" <> value "ghc-8.4.3")

main :: IO ()
main = do
  (opts, ()) <- parseOpts
  code <- newEmptyTMVarIO
  result <- newTVarIO $ Result 0 "Haven't run any code yet, get started!" False
  let app = App
        { appCode = code
        , appResult = result
        , appSnapshot = optsSnapshot opts
        }
      builder' = do
        eres <- tryAny $ builder app
        atomically $ setResult app
          (case eres of
            Left e -> "Code builder crashed, sorry!\n\n" <> fromString (show e)
            Right void -> absurd void)
          False

  withAsync builder' $ const $
    case optsPort opts of
      Just port -> warp port app
      Nothing -> toWaiApp app >>= run
