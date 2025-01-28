{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth (TokenStore, initTokenStore, register, verifyAdmin)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (unpack)
import Data.Text.Lazy (pack, toStrict)
import Data.UUID (toText)
import GHC.Conc (atomically)
import Network.HTTP.Types.Status (badRequest400, unauthorized401)
import Proc (Proc, call, launch)
import System.Environment (getArgs)
import Web.Scotty (ActionM, finish, header, notFound, pathParam, pathParamMaybe, post, scotty, status, text)

main :: IO ()
main = getArgs >>= launch >>= runWithProc
  where
    runWithProc proc = initTokenStore >>= runWebServer proc

runWebServer :: Proc -> TokenStore -> IO ()
runWebServer proc ts =
  scotty 8080 $ do
    post "/raw/:text" $ requireAdmin ts >> pathParam "text" >>= routeRaw
    post "/pane/:pane/create" $ pathParamMaybe "pane" >>= expectJust >>= registerPane >>= callCreate
    notFound $ text "404\n"
  where
    callAct cmd = liftIO $ call proc cmd
    routeRaw cmd = callAct cmd >>= text . pack

    expectJust Nothing = status badRequest400 >> text "missing required param." >> finish
    expectJust (Just x) = return x

    registerPane name =
      (liftIO . register ts) name >>= \case
        Just uuid -> (text . pack . unpack . toText) uuid >> return name
        Nothing -> status badRequest400 >> text "couldn't create pane." >> finish

    -- TODO: validation. if name contains a newline, there's a potential for
    -- unauthenticated command injection.
    callCreate name = void $ liftIO . callAct $ "CREATE: " ++ unpack name

requireAdmin :: TokenStore -> ActionM ()
requireAdmin ts = header "Auth" >>= isOk >>= verify
  where
    isOk (Just token) = liftIO $ atomically $ verifyAdmin ts $ toStrict token
    isOk Nothing = return False

    verify True = return ()
    verify False = status unauthorized401 >> finish
