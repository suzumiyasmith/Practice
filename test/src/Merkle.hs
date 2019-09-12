module Merkle where

import Data.List as L
import Crypto.Hash
import Data.ByteArray
import qualified Data.ByteString as B

import Data.Maybe

import Data.Tree
import Data.Hex
import qualified Data.ByteString.Char8 as BC

type Merkle a h = Maybe (M a h)
data M a h = Branch h (M a h) (M a h) | Leaf h a
  deriving Show

toMerkleProof :: Eq a => a -> M a h -> [Proof h]
toMerkleProof a m = L.reverse $ S (getHash m) : toMerkleProof' a m

data Proof h = L h | R h | S h
  deriving (Show ,Eq, Functor)

toMerkleProof' :: Eq a => a -> M a h -> [Proof h]
toMerkleProof' a (Branch h m1 m2) =
  case (toMerkleProof' a m1, toMerkleProof' a m2) of
    ([], []) -> []
    (p1, []) -> L (getHash m2) : p1
    ([], p2) -> R (getHash m1) : p2
toMerkleProof' a (Leaf h b) = if a == b then [S h] else []

validateMerkleProof :: [Proof B.ByteString] -> Bool
validateMerkleProof [] = True
validateMerkleProof [h] = True
validateMerkleProof [h1, h2] = h1 == h2
validateMerkleProof (S h1 : L h2 : hs) = validateMerkleProof (S h3 : hs) where h3 = hashIt (append h1 h2)
validateMerkleProof (S h1 : R h2 : hs) = validateMerkleProof (S h3 : hs) where h3 = hashIt (append h2 h1)

toTree :: M a h -> Tree (h, Maybe a)
toTree (Leaf h a) = Node (h, Just a) []
toTree (Branch h m1 m2) = Node (h, Nothing) $ toTree <$> [m1, m2]

getHash :: M a h -> h
getHash (Branch h _ _) = h
getHash (Leaf h _) = h

toM :: ByteArrayAccess a => [a] -> Merkle a B.ByteString
toM [] = Nothing
toM [aa] = return $ Leaf (hashIt aa) aa
toM as = let
  len = L.length as
  l = powerSplitNum len
  in  if l == len
        then toM as
        else do
          let (vsl, vsr) = L.splitAt l as
          hl <- toM vsl
          hr <- toM vsr
          return $ Branch (hashIt $ append (getHash hl) (getHash hr)) hl hr

powerSplitNum :: Int -> Int
powerSplitNum i = head $ dropWhile (< i `div` 2) $ (2 ^) <$> [0..]

hexHash :: (Hex h) => M a h -> M a h
hexHash (Leaf h a) = Leaf (hex h) a
hexHash (Branch h m1 m2) = Branch (hex h) (hexHash m1) (hexHash m2)

hashIt :: ByteArrayAccess a => a -> B.ByteString
hashIt =  B.pack . unpack . hashWith SHA256

test1 = fromJust $ toM $ d2 16
showTest = drawTree $ show <$> toTree (hexHash test1)

d n = B.pack <$> L.replicate n (fromIntegral <$> [1..10])
d2 n = BC.pack . show <$> [1..16]

test2 = toMerkleProof (BC.pack $ show 16) (hexHash test1)

test3 :: Int -> String
test3 n = show $ fmap hex <$> toMerkleProof (BC.pack $ show n) (test1)
