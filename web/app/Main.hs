{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Auth (AuthStatus (..), TokenStore, checkAuth, initTokenStore, register, unregister, unregisterAllFrom, verifyAdmin)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Lazy as L
import Data.UUID (toText)
import GHC.Conc (atomically)
import Network.HTTP.Types.Status (conflict409, forbidden403, internalServerError500, unauthorized401)
import Proc (Proc, call, kill, launch, mkCommand, mkComp, mkUnvalidatedCommand)
import System.Environment (getArgs)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import Web.Scotty (ActionM, ScottyM, capture, delete, finish, header, notFound, pathParam, post, queryParam, scotty, status, text)

foreign import ccall "reallyExit" reallyExit :: IO ()

main :: IO ()
main = getArgs >>= launch >>= setupAndRun
  where
    setupAndRun proc = setupTerm proc >> runWithProc proc
    runWithProc proc = initTokenStore >>= runWebServer proc

    setupTerm proc = installHandler sigINT (sigintHandler proc) Nothing
    sigintHandler proc =
      Catch $
        putStrLn "Exiting." >> kill proc >> reallyExit

runWebServer :: Proc -> TokenStore -> IO ()
runWebServer proc ts =
  scotty 8080 $ do
    post "/raw/:text" $ requireAdmin ts >> pathParam "text" >>= routeRaw
    post "/removefrom/:peer" $ requireAdmin ts >> pathParam "peer" >>= routeRemoveFrom
    post "/pane/:pane/create" $ do
      pane <- pathParam "pane"
      color <- queryParam "color"
      liftIO $ putStrLn color
      callCreate pane color

      peer <- header "X-Forwarded-For"
      let peer' = L.unpack $ fromMaybe "NO PEER" peer

      registerPane peer' (T.pack pane)

    makeDrawRoute "RECT" "rect" ["color", "x", "y", "w", "h"]
    makeDrawRoute "CIRCLE" "circle" ["color", "x", "y", "r"]
    makeDrawRoute "LINE" "line" ["color", "x", "y", "x2", "y2"]
    makeDrawRoute "COPY_RECT" "copy_rect" ["x", "y", "x2", "y2", "w", "h"]
    makeDrawRoute "BEZIER2" "bezier2" ["color", "x0", "y0", "x1", "y1", "x2", "y2"]
    makeDrawRoute "TRIANGLE" "triangle" ["color", "x0", "y0", "x1", "y1", "x2", "y2"]

    delete "/pane/:pane" $
      pathParam "pane" >>= checkAuthScotty >>= \pane ->
        callDelete (T.unpack pane) >> liftIO (unregister ts pane)

    notFound $ text "404\n"
  where
    makeDrawRoute :: String -> String -> [String] -> ScottyM ()
    makeDrawRoute cmd slug args = post (capture $ "/pane/:pane/" ++ slug) $ do
      pane <- pathParam "pane"
      vals <- mapM (queryParam . pack) args
      _ <- checkAuthScotty pane
      callCmd $ mkCommand ((mkComp . T.unpack) pane) (mkComp cmd) (Prelude.map mkComp vals)

    callAct cmd = liftIO $ call proc cmd
    routeRaw cmd = (callAct . mkUnvalidatedCommand) cmd >>= text . pack

    routeRemoveFrom :: String -> ActionM ()
    routeRemoveFrom peer =
      liftIO (unregisterAllFrom ts peer) >>= mapM_ (callDelete . T.unpack)

    registerPane peer name =
      liftIO (register ts peer name) >>= \case
        Just uuid -> (text . pack . T.unpack . toText) uuid
        -- If we're here, this means we've already created the pane in the C
        -- layer. If the pane already exists in our memory, then, we've done
        -- something wrong.
        Nothing -> status internalServerError500 >> text "Pane with same name exists." >> finish

    checkAuthScotty name = header "Auth" >>= check >>= serve
      where
        check Nothing = status unauthorized401 >> finish
        check (Just uuid) =
          (liftIO . atomically) (verifyAdmin ts $ toStrict uuid) >>= \case
            True -> return Allowed
            False -> liftIO $ checkAuth ts name uuid

        serve Allowed = return name
        serve Disallowed = status forbidden403 >> finish
        serve BadToken = status unauthorized401 >> finish

    callCreate name color = callCmd $ mkCommand (mkComp name) (mkComp "CREATE") [mkComp color]

    callDelete name = callCmd $ mkCommand (mkComp name) (mkComp "REMOVE") []

    callCmd cmd =
      callAct cmd >>= \case
        "OK" -> return ()
        x -> setStatusFromErr x >> (text . pack) x >> finish

    setStatusFromErr "act_create: failed: duplicate pane" = status conflict409
    setStatusFromErr _ = status internalServerError500

requireAdmin :: TokenStore -> ActionM ()
requireAdmin ts = header "Auth" >>= isOk >>= verify
  where
    isOk (Just token) = liftIO $ atomically $ verifyAdmin ts $ toStrict token
    isOk Nothing = return False

    verify True = return ()
    verify False = status unauthorized401 >> finish
