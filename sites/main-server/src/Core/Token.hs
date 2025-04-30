{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Core.Token where

import Quaalude
import Core.Config (ServerKey)
import Crypto.Cipher.Types (KeySizeSpecifier(..), cipherKeySize, cfbEncrypt, cfbDecrypt, nullIV, cipherInit)
import Crypto.Cipher.AES (AES256)
import Crypto.Error (CryptoFailable(..))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

iv = nullIV @AES256
splitChar = '\0'

type Token = BS8.ByteString
type SToken = String

-- | Encryption methods require specific key size. This will pad/shorten the server key as necessary
-- by dynamically setting the key size, we can be sure the size is correct (if the algorithm is)
{-# INLINE toCipher #-}
toCipher ∷ ServerKey → AES256
toCipher sk = cipher
    where
        shortBy = (size -) $ BS8.length $ sk
        size = case cipherKeySize (undefined ∷ AES256) of
          KeySizeRange _ max' → max'
          KeySizeEnum xs → last xs
          KeySizeFixed size' → size'
        key
          | shortBy > 0 = sk <> BS8.replicate shortBy splitChar
          | otherwise = BS8.take size sk
        CryptoPassed cipher ∷ CryptoFailable AES256 = cipherInit key

tokenize ∷ ServerKey → BS8.ByteString → Token
tokenize sk msg = token
    where
        encrypted = cfbEncrypt (toCipher sk) iv $ BS8.take 32 $ msg <> BS8.replicate 32 splitChar
        token = Base16.encode encrypted

untokenize ∷ ServerKey → Token → BS8.ByteString
untokenize sk token = msg
    where
        msg = BS8.takeWhile (/= splitChar) $ cfbDecrypt (toCipher sk) iv $ fst $ Base16.decode token

validateToken ∷ ∀ truth. (FromJSON truth, Eq truth) ⇒ ServerKey → Token → truth → Bool
validateToken sk token truth = maybe False (== truth) decodedToken
    where
      decodedToken = decode $ BSL8.fromStrict $ untokenize sk token

stokenize ∷ ServerKey → String → SToken
stokenize sk = BS8.unpack . tokenize sk . BS8.pack

suntokenize ∷ ServerKey → SToken → String
suntokenize sk = BS8.unpack . untokenize sk . BS8.pack

toSToken ∷ Token → SToken
toSToken = BS8.unpack

toToken ∷ SToken → Token
toToken = BS8.pack
