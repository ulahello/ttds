{- cabal:
build-depends: base, scrypt, bytestring
-}

import Crypto.Scrypt
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)

main = do
  args <- getArgs
  pass <- encryptPassIO defaultParams $ (Pass . B.pack . head) args
  putStrLn $ B.unpack $ getEncryptedPass pass
