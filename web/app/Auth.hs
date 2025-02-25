{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth (register, unregister, checkAuth, TokenStore, initTokenStore, verifyAdmin) where

import Control.Concurrent.STM (STM, modifyTVar, newTVarIO)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Exception (Exception, throwIO)
import Control.Monad.STM (atomically)
import Crypto.Scrypt (EncryptedPass (..), Pass (..), verifyPass')
import Data.ByteString.Char8 (pack)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as L
import Data.UUID (UUID, fromText)
import Data.UUID.V4 (nextRandom)

data BadPassfileException = WrongSpec | CantDecode deriving (Show)

data RequestException = BadTokenException deriving (Show)

instance Exception BadPassfileException

instance Exception RequestException

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

checkAuth :: TokenStore -> Name -> L.Text -> IO Bool
checkAuth ts name t = case fromText $ L.toStrict t of
  Just token -> atomically $ checkAuth' token
  Nothing -> throwIO BadTokenException
  where
    checkAuth' :: UUID -> STM Bool
    checkAuth' uuid = check uuid . tokens <$> readTVar ts
    check uuid toks = case toks Map.!? name of
      Just x -> x == uuid
      Nothing -> False

unregister :: TokenStore -> Name -> IO ()
unregister ts name = atomically $ modifyTVar ts unregister'
  where
    unregister' (TokenStoreInner {adminCred = admin, tokens = toks}) =
      let toks' = Map.delete name toks
       in TokenStoreInner {adminCred = admin, tokens = toks'}

register :: TokenStore -> Name -> IO (Maybe Token)
register ts name = nextRandom >>= atomically . register'
  where
    register' uuid = readTVar ts >>= tryUpdate uuid
    tryUpdate :: Token -> TokenStoreInner -> STM (Maybe Token)
    tryUpdate tok ts' = case tokens ts' Map.!? name of
      Just _ -> return Nothing
      Nothing -> modifyTVar ts (withTok tok) >> return (Just tok)

    withTok tok tsi = TokenStoreInner {adminCred = adminCred tsi, tokens = Map.insert name tok $ tokens tsi}

verifyAdmin :: TokenStore -> Text -> STM Bool
verifyAdmin ts tok = readTVar ts <&> verify
  where
    verify ts' = verifyPass' (Pass {getPass = encodeUtf8 tok}) (adminCred ts')
