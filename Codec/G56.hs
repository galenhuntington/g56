--  Implements the G56 encoding.

{-# LANGUAGE CPP #-}

module Codec.G56 (alphabet, encode, decode) where

import Data.Int (Int64)
import Data.String (fromString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import qualified Data.Vector.Unboxed as V
import Data.Foldable (foldl')
import Data.List (unfoldr, mapAccumL)
import Data.Tuple (swap)
import Data.Char
import Data.Maybe


#define __ True

alphabet :: ByteString
alphabet = fromString
   "0123456789ABCDEFGHJKLMNPQRSTVWXYZabcdefghjklmnpqrstvwxyz"

emap :: V.Vector Int
emap = V.fromListN 56 $ map fromEnum $ B.unpack alphabet

dmap :: V.Vector Int64
dmap = V.replicate 127 (-1) `V.update` V.map swap' (V.indexed emap)
   where swap' (a, b) = (b, fromIntegral a)


coeffs :: V.Vector Int64
coeffs = V.fromListN 5 [12*56^5, 2*56^4, 24*56^2, 5*56, 1]


--  Reverses a linked list, so not that efficient.
{-# INLINE toDigits #-}
toDigits :: Int64 -> Int -> Int64 -> [Int64]
toDigits base digs = reverse . take digs
   . unfoldr (\x -> Just $ swap (x `divMod` base))

intToCode :: Int -> Int64 -> String
intToCode sz = map toB . take (sz + if sz<3 then 1 else 2) . toDigits 56 7
   where toB n = chr $ emap `V.unsafeIndex` fromIntegral n

codeToDigits :: String -> [Int64]
codeToDigits = filter (>=0) . mapMaybe ((dmap V.!?) . fromEnum)

digitsToInt :: [Int64] -> Int64
digitsToInt = foldl' (\ n b -> n*56 + b) 0 . take 7 . (++ repeat 0)

intToBytes :: Int -> Int64 -> Builder
intToBytes sz =
   mconcat . take (sz - if sz<4 then 1 else 2) . map (word8 . fromIntegral)
      . snd . flip (mapAccumL ((swap .) . divMod)) (V.toList coeffs)

bytesToInt :: ByteString -> Int64
bytesToInt bs = sum
   $ zipWith (*) (V.toList coeffs)
   $ map fromIntegral (B.unpack bs) ++ repeat 0


--  Main event.
--  Strings for ASCII, as, however flawed, it's a Haskell lingua franca.
--  Strict ByteStrings only for now.

encode :: ByteString -> String
encode = mconcat . loop where
   loop x | B.null x = []
          | __       = intToCode (B.length a) (bytesToInt a) : loop b
      where (a, b) = B.splitAt 5 x

decode :: String -> ByteString
decode = toStrict . toLazyByteString . mconcat . loop . codeToDigits where
   loop x | null x = []
          | __     = intToBytes (length a) (digitsToInt a) : loop b
      where (a, b) = splitAt 7 x

