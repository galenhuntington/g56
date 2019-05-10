import qualified Data.ByteString as B
import Control.Monad (when, unless)
import Data.String (fromString)
import Test.QuickCheck

import qualified Codec.G56 as G56

instance Arbitrary B.ByteString where arbitrary = fmap fromString arbitrary

prop_inv :: B.ByteString -> Bool
prop_inv b = G56.decode (G56.encode b) == b

--  This is the only test that checks "loose" decoding.
prop_seg :: Int -> B.ByteString -> Bool
prop_seg i b = G56.decode (take (i `mod` (length enc + 1)) enc) `B.isPrefixOf` b
   where enc = G56.encode b

prop_lex :: B.ByteString -> B.ByteString -> Bool
prop_lex b1 b2 = (G56.encode b1 `compare` G56.encode b2) == b1 `compare` b2

prop_len :: B.ByteString -> Bool
prop_len b = length (G56.encode b) == ceiling (7 * fromIntegral (B.length b) / 5)

piBytes :: B.ByteString
piBytes = B.pack [
   0x24, 0x3f, 0x6a, 0x88, 0x85, 0xa3, 0x08, 0xd3,
   0x13, 0x19, 0x8a, 0x2e, 0x03, 0x70, 0x73, 0x44,
   0xa4, 0x09, 0x38, 0x22, 0x29, 0x9f, 0x31, 0xd0,
   0x08, 0x2e, 0xfa, 0x98, 0xec, 0x4e, 0x6c, 0x89 ]

main = do
   let checkeq err a b = do
         putStr (show a) >> putStr " == " >> print b
         when (a/=b) $ error err
   let check err test = flip when (error err) =<< do
         not . isSuccess <$> test

   --  Check examples from spec.
   checkeq "Spec example 1 failed."
      (G56.encode (fromString "Hello, world!")) "FTbpRez9R8v9x2PBZE8"
   checkeq "Spec example 2 failed."
      (G56.encode piBytes) "7kEndAMbwHbRgRWad1S23Eer3x0b8sfMJZ8A0x9W5NPCt"

   --  Encode/decode are inverses.
   check "Inversion failure." $
      quickCheckWithResult stdArgs {maxSuccess = 10000} prop_inv

   --  Length property.
   check "Length failure." $
      quickCheckWithResult stdArgs {maxSuccess = 10000} prop_len

   --  Lex order property.
   check "Lexicographic order failure." $
      quickCheckWithResult stdArgs {maxSuccess = 10000} prop_lex

   --  Initial segment property.
   check "Initial segment failure." $
      quickCheckWithResult stdArgs {maxSuccess = 10000} prop_seg

   --  Invalid characters such as whitespace are ignored.
   checkeq "Invalid character not ignored."
      (G56.decode "ab\rcd'' \\ ef \n") (G56.decode "abcdef")

