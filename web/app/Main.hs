{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import Proc (Proc, call, launch)
import System.Environment (getArgs)
import Web.Scotty (notFound, pathParam, post, scotty, text)

main :: IO ()
main = getArgs >>= launch >>= runWebServer

runWebServer :: Proc -> IO ()
runWebServer proc =
  scotty 8080 $ do
    post "/raw/:text" $
      pathParam "text" >>= \cmd ->
        liftIO (call proc cmd)
          >>= text . pack

    notFound $ text "404\n"
