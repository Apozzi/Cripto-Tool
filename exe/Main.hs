module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString as B
import Crypto.Hash
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.MD4 as MD4
import qualified Crypto.Hash.MD2 as MD2
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512

main :: IO ()
main = do
    args <- getArgs
    case args of
        [op, input, "-k", key, "-o", outputFile] -> processWithKeyAndOutput op input key outputFile
        [op, input, "-k", key] -> processWithKey op input key
        [op, input, "-o", outputFile] -> processWithOutput op input outputFile
        [op, input] -> processSimple op input
        _ -> putStrLn "Uso: programa <operação> <texto ou -iARQUIVO> [-k CHAVE] [-o ARQUIVO_SAIDA]"

processWithKeyAndOutput :: String -> String -> String -> String -> IO ()
processWithKeyAndOutput op input key outputFile = do
    text <- readInputText input
    result <- processOperation op text (Just key)
    writeFile outputFile result
    putStrLn $ "Resultado escrito em " ++ outputFile

processWithKey :: String -> String -> String -> IO ()
processWithKey op input key = do
    text <- readInputText input
    result <- processOperation op text (Just key)
    putStrLn result

processWithOutput :: String -> String -> String -> IO ()
processWithOutput op input outputFile = do
    text <- readInputText input
    result <- processOperation op text Nothing
    writeFile outputFile result
    putStrLn $ "Resultado escrito em " ++ outputFile

processSimple :: String -> String -> IO ()
processSimple op input = do
    text <- readInputText input
    result <- processOperation op text Nothing
    putStrLn result

readInputText :: String -> IO String
readInputText input = if "-i" `isPrefixOf` input
                      then readFile (drop 2 input)
                      else return input

processOperation :: String -> String -> Maybe String -> IO String
processOperation op text maybeKey = return $ case op of
    "encode-binary" -> encodeBinary text
    "decode-binary" -> decodeBinary text
    "encode-base64" -> encodeBase64 text
    "decode-base64" -> decodeBase64 text
    "caesar-encrypt" -> caesarCipher (maybe 3 read maybeKey) text
    "caesar-decrypt" -> caesarCipher (maybe (-3) (negate . read) maybeKey) text
    "rot13" -> rot13 text
    "encode-hex" -> encodeHex text
    "decode-hex" -> decodeHex text
    "encode-MD5" -> encodeMD5 text
    "encode-MD4" -> encodeMD4 text
    "encode-MD2" -> encodeMD2 text
    "encode-SHA256" -> encodeSHA256 text
    "encode-SHA512" -> encodeSHA512 text
    "aes-encrypt" -> aesEncrypt text (maybe "" id maybeKey)
    "aes-decrypt" -> aesDecrypt text (maybe "" id maybeKey)
    "des-encrypt" -> desEncrypt text (maybe "" id maybeKey)
    "des-decrypt" -> desDecrypt text (maybe "" id maybeKey)
    "xor-encrypt" -> xorCipher text (maybe "" id maybeKey)
    "xor-decrypt" -> xorDecipher text (maybe "" id maybeKey)
    _ -> "Operação inválida."

encodeHash :: (B.ByteString -> B.ByteString) -> String -> String
encodeHash hash str = BC.unpack $ B16.encode $ hash $ BC.pack str

encodeMD5 :: String -> String
encodeMD5 = encodeHash MD5.hash

encodeMD2 :: String -> String
encodeMD2 = encodeHash MD2.hash

encodeMD4 :: String -> String
encodeMD4 = encodeHash MD4.hash

encodeSHA256 :: String -> String
encodeSHA256 = encodeHash SHA256.hash

encodeSHA512 :: String -> String
encodeSHA512 = encodeHash SHA512.hash

encodeBinary :: String -> String
encodeBinary = unwords . map (toBinary . ord)

decodeBinary :: String -> String
decodeBinary = map (chr . fromBinary) . words

xorCipher :: String -> String -> String
xorCipher text key =
    let textBS = BC.pack text
        keyBS = BC.pack $ cycle key
        result = B.pack $ B.zipWith xor textBS keyBS
    in BC.unpack $ B64.encode result

xorDecipher :: String -> String -> String
xorDecipher ciphertext key =
    let decodeOrError = either (error . ("Erro na decodificação Base64: " ++)) id
        cipherBS = decodeOrError $ B64.decode $ BC.pack ciphertext
        keyBS = BC.pack $ cycle key
        result = B.pack $ B.zipWith xor cipherBS keyBS
    in BC.unpack result

cryptoOperation :: Int -> (B.ByteString -> B.ByteString) -> String -> String -> String
cryptoOperation keySize op text key =
    let textBS = BC.pack text
        keyBS = BC.pack $ take keySize $ key ++ repeat '\0'
        result = B.pack $ B.zipWith xor (B.concat $ repeat textBS) (B.concat $ repeat keyBS)
    in BC.unpack $ op result

encrypt :: Int -> String -> String -> String
encrypt keySize = cryptoOperation keySize B64.encode

decrypt :: Int -> String -> String -> String
decrypt keySize ciphertext key =
    let decodeOrError = either (error . ("Erro na decodificação Base64: " ++)) id
        decodeOp = decodeOrError . B64.decode
    in cryptoOperation keySize decodeOp ciphertext key

aesEncrypt, aesDecrypt :: String -> String -> String
aesEncrypt = encrypt 16
aesDecrypt = decrypt 16

desEncrypt, desDecrypt :: String -> String -> String
desEncrypt = encrypt 8
desDecrypt = decrypt 8

toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse $ go n
  where
    go 0 = []
    go n = let (q, r) = n `divMod` 2 in intToDigit r : go q

fromBinary :: String -> Int
fromBinary = foldl (\acc x -> acc * 2 + digitToInt x) 0


caesarCipher :: Int -> String -> String
caesarCipher shift = map (shiftChar shift)
  where
    shiftChar n c
      | isAsciiLower c = chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
      | isAsciiUpper c = chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
      | otherwise = c

rot13 :: String -> String
rot13 = caesarCipher 13

encodeHex :: String -> String
encodeHex = BC.unpack . Hex.encode . BC.pack

decodeHex :: String -> String
decodeHex hexString = case Hex.decode (BC.pack hexString) of
    Right bs -> BC.unpack bs
    Left err -> "Erro na decodificação hex: " ++ show err


encodeBase64 :: String -> String
encodeBase64 = BC.unpack . B64.encode . BC.pack

decodeBase64 :: String -> String
decodeBase64 text = case B64.decode $ BC.pack text of
    Left err -> "Erro na decodificação: " ++ err
    Right bs -> BC.unpack bs
