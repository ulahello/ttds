{-# LANGUAGE OverloadedStrings #-}

module Auth (TokenStore, initTokenStore, verifyToken, verifyAdmin) where

import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import qualified Data.Text.Lazy as Data.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Conc (STM, TVar, readTVar)
import GHC.Conc.Sync (newTVarIO)

type Token = Data.Lazy.Text -- UUID

--  A named resource. Likely a pane.
type Name = String

type Hashed = ByteString

type TokenStore = TVar TokenStoreInner

newtype TokenStoreInner = TokenStoreInner
  {adminTok :: Hashed}

initTokenStore :: IO TokenStore
initTokenStore = newTVarIO $ TokenStoreInner {adminTok = "hardcoded-lmao"}

verifyToken :: TokenStore -> Token -> Name -> STM Bool
verifyToken _ _ _ = return False

verifyAdmin :: TokenStore -> Token -> STM Bool
verifyAdmin ts tok = readTVar ts <&> (== hash tok) . adminTok

hash :: Token -> Hashed
hash = encodeUtf8
