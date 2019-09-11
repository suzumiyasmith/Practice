module Merkle where

import Data.List as L
import Crypto.Hash
import Data.ByteArray
import qualified Data.ByteString as B

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.Tree
import Data.Hex
import qualified Data.ByteString.Char8 as BC

type Merkle a h = Maybe (M a h)
data M a h = Branch h (M a h) (M a h) | Leaf h a
  deriving Show

toTree :: M a h -> Tree (h, Maybe a)
toTree (Leaf h a) = Node (h, Just a) []
toTree (Branch h m1 m2) = Node (h, Nothing) $ toTree <$> [m1, m2]

getHash :: M a h -> h
getHash (Branch h _ _) = h
getHash (Leaf h _) = h

toM :: ByteArrayAccess a => [a] -> M a B.ByteString
toM [] = undefined
toM [aa] = Leaf (B.pack $ unpack $ hashWith SHA256 aa) aa
toM as = let
  len = L.length as
  l = powerSplitNum len
  in  if l == len
        then toM as 
        else let
          (vsl, vsr) = L.splitAt l as
          hl = toM vsl
          hr = toM vsr
          in Branch (B.pack $ unpack $ hashWith SHA256 $ append (getHash hl) (getHash hr)) hl hr

powerSplitNum :: Int -> Int
powerSplitNum i = head $ dropWhile (< i `div` 2) $ (\x -> 2 ^ x) <$> [0..]

hexHash :: (Hex h) => M a h -> M a h
hexHash (Leaf h a) = (Leaf (hex h) a)
hexHash (Branch h m1 m2) = Branch (hex h) (hexHash m1) (hexHash m2)

test1 = toM $ d2 16
showTest = drawTree $ show <$> toTree (hexHash test1)

d n = B.pack <$> L.replicate n (fromIntegral <$> [1..10])
d2 n = BC.pack <$> show <$> [1..16]