-- stack --resolver lts-16.27 script --package cryptohash,bytestring --optimize
{-# LANGUAGE OverloadedStrings #-}

import           Crypto.Hash.MD5
import           Data.Bits                      ( (.&.)
                                                , shiftR
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Char                      ( intToDigit )
import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )
import           Data.Word                      ( Word8 )


-- The following three functions are stolen
byteHex :: Word8 -> String
byteHex b = map intToDigit [fromIntegral b `shiftR` 4, fromIntegral b .&. 0xf]

showHex :: [Word8] -> String
showHex bs = concatMap byteHex bs

hashString :: String -> String
hashString s = showHex $ B.unpack $ hash $ BC.pack s

-- Here starts "my" code.
testNonce :: String -> Int -> Int -> Bool
testNonce key n nonce = replicate n '0' == take n digest
  where digest = hashString $ key ++ show (nonce)

search :: String -> Int -> Int
search key n = fromJust $ find (testNonce key n) [0 ..]

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn content
  putStrLn $ "Part 1: " ++ show (search content 5)
  putStrLn $ "Part 2: " ++ show (search content 6)


