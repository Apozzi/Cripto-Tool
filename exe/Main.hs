module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Bits
import Data.List
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as BL
import qualified Data.Hex as Hex

main :: IO ()
main = do
    args <- getArgs
    case args of
        [op, input, "-o", outputFile] -> do
            text <- if "-i" `isPrefixOf` input
                        then readFile (drop 2 input)
                        else return input
            result <- processOperation op text
            writeFile outputFile result
            putStrLn $ "Resultado escrito em " ++ outputFile
        [op, input] -> do
            text <- if "-i" `isPrefixOf` input
                        then readFile (drop 2 input)
                        else return input
            result <- processOperation op text
            putStrLn result
        _ -> putStrLn "Uso: programa <operação> <texto ou -iARQUIVO> [-o ARQUIVO_SAIDA]"

processOperation :: String -> String -> IO String
processOperation op text = return $ case op of
    "encode-binary" -> encodeBinary text
    "decode-binary" -> decodeBinary text
    "encode-base64" -> encodeBase64 text
    "decode-base64" -> decodeBase64 text
    "caesar-encrypt" -> caesarCipher 3 text
    "caesar-decrypt" -> caesarCipher (-3) text
    "rot13" -> rot13 text
    "md5" -> md5 text
    "sha256" -> sha256 text
    "encode-hex" -> encodeHex text
    "decode-hex" -> decodeHex text
    _ -> "Operação inválida."

encodeBinary :: String -> String
encodeBinary = unwords . map (toBinary . ord)

decodeBinary :: String -> String
decodeBinary = map (chr . fromBinary) . words

toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse $ go n
  where
    go 0 = []
    go n = let (q, r) = n `divMod` 2 in intToDigit r : go q

fromBinary :: String -> Int
fromBinary = foldl (\acc x -> acc * 2 + digitToInt x) 0

encodeBase64 :: String -> String
encodeBase64 = BC.unpack . B64.encode . BC.pack

decodeBase64 :: String -> String
decodeBase64 text = case B64.decode $ BC.pack text of
    Left err -> "Erro na decodificação: " ++ err
    Right bs -> BC.unpack bs

caesarCipher :: Int -> String -> String
caesarCipher shift = map (shiftChar shift)
  where
    shiftChar n c
      | isAsciiLower c = chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
      | isAsciiUpper c = chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
      | otherwise = c

rot13 :: String -> String
rot13 = caesarCipher 13

md5 :: String -> String
md5 = show . Hash.hash . BC.pack

sha256 :: String -> String
sha256 = show . (Hash.hash :: BC.ByteString -> Hash.Digest Hash.SHA256) . BC.pack

encodeHex :: String -> String
encodeHex = Hex.hex . BC.pack

decodeHex :: String -> String
decodeHex hexString = case Hex.unhex $ BC.pack hexString of
    Right bs -> BC.unpack bs
    Left err -> "Erro na decodificação hex: " ++ err