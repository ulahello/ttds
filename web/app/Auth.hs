{-# LANGUAGE OverloadedStrings #-}

module Auth (register, TokenStore, initTokenStore, verifyToken, verifyAdmin) where

import Control.Concurrent.STM (STM, modifyTVar, newTVarIO)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Exception (Exception, throwIO)
import Control.Monad.STM (atomically)
import Crypto.Scrypt (EncryptedPass (..), Pass (..), verifyPass')
import Data.ByteString.Char8 (pack)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

data BadPassfileException = WrongSpec | CantDecode deriving (Show)

instance Exception BadPassfileException

type Token = UUID

--  A named resource. Likely a pane.
type Name = Text

type TokenStore = TVar TokenStoreInner

data TokenStoreInner = TokenStoreInner
  { adminCred :: EncryptedPass,
    tokens :: Map.Map Name Token
  }

initTokenStore :: IO TokenStore
initTokenStore =
  readFile "admin.pass" >>= getCreds >>= \cred ->
    newTVarIO $
      TokenStoreInner
        { adminCred = EncryptedPass {getEncryptedPass = cred},
          tokens = Map.empty
        }
  where
    getCreds file = parse $ lines file
    parse [cred] = return $ pack cred
    parse _ = throwIO WrongSpec

register :: TokenStore -> Name -> IO (Maybe Token)
register ts name = nextRandom >>= atomically . register'
  where
    register' uuid = readTVar ts >>= tryUpdate uuid
    tryUpdate :: Token -> TokenStoreInner -> STM (Maybe Token)
    tryUpdate tok ts' = case tokens ts' Map.!? name of
      Just _ -> return Nothing
      Nothing -> modifyTVar ts (withTok tok) >> return (Just tok)

    withTok tok tsi = TokenStoreInner {adminCred = adminCred tsi, tokens = Map.insert name tok $ tokens tsi}

verifyToken :: TokenStore -> Token -> Name -> STM Bool
verifyToken _ _ _ = return False

verifyAdmin :: TokenStore -> Text -> STM Bool
verifyAdmin ts tok = readTVar ts <&> verify
  where
    verify ts' = verifyPass' (Pass {getPass = encodeUtf8 tok}) (adminCred ts')
