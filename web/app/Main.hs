{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth (TokenStore, initTokenStore, verifyAdmin)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack, toStrict)
import GHC.Conc (atomically)
import Network.HTTP.Types.Status (unauthorized401)
import Proc (Proc, call, launch)
import System.Environment (getArgs)
import Web.Scotty (ActionM, finish, header, notFound, pathParam, post, scotty, status, text)

main :: IO ()
main = getArgs >>= launch >>= runWithProc
  where
    runWithProc proc = initTokenStore >>= runWebServer proc

runWebServer :: Proc -> TokenStore -> IO ()
runWebServer proc ts =
  scotty 8080 $ do
    post "/raw/:text" $ requireAdmin ts >> pathParam "text" >>= routeRaw
    notFound $ text "404\n"
  where
    routeRaw cmd = liftIO (call proc cmd) >>= text . pack

requireAdmin :: TokenStore -> ActionM ()
requireAdmin ts = header "Auth" >>= isOk >>= verify
  where
    isOk (Just token) = liftIO $ atomically $ verifyAdmin ts $ toStrict token
    isOk Nothing = return False

    verify True = return ()
    verify False = status unauthorized401 >> finish
