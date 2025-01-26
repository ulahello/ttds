{-# LANGUAGE OverloadedStrings #-}

module Auth (TokenStore, initTokenStore, verifyToken, verifyAdmin) where

import Control.Exception (Exception, throwIO)
import Crypto.Scrypt (EncryptedPass (..), Pass (..), verifyPass')
import Data.ByteString.Char8 (pack)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Conc (STM, TVar, readTVar)
import GHC.Conc.Sync (newTVarIO)

data BadPassfileException = WrongSpec | CantDecode deriving (Show)

instance Exception BadPassfileException

type Token = Text -- UUID

--  A named resource. Likely a pane.
type Name = String

type TokenStore = TVar TokenStoreInner

newtype TokenStoreInner = TokenStoreInner
  {adminCred :: EncryptedPass}

initTokenStore :: IO TokenStore
initTokenStore =
  readFile "admin.pass" >>= getCreds >>= \cred ->
    newTVarIO $
      TokenStoreInner
        { adminCred = EncryptedPass {getEncryptedPass = cred}
        }
  where
    getCreds file = parse $ lines file
    parse [cred] = return $ pack cred
    parse _ = throwIO WrongSpec

verifyToken :: TokenStore -> Token -> Name -> STM Bool
verifyToken _ _ _ = return False

verifyAdmin :: TokenStore -> Token -> STM Bool
verifyAdmin ts tok = readTVar ts <&> verify
  where
    verify ts' = verifyPass' (Pass {getPass = encodeUtf8 tok}) (adminCred ts')
