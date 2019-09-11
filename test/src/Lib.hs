module Lib where

import Data.List as L
import Crypto.Hash
import Data.ByteArray
import qualified Data.ByteString as B

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data M a h = Branch h (M a h) (M a h) | Leaf h a
  deriving Show

type Err = String

type Algo m = (Monad m, MonadError Err m, MonadWriter String m)

getHash :: M a h -> h
getHash (Branch h _ _) = h
getHash (Leaf h _) = h

toM :: (ByteArrayAccess a, Algo m) => [a] -> m (M a B.ByteString)
toM [aa] = return $ Leaf (B.pack $ unpack $ hashWith SHA256 aa) aa
toM as = case powerSplit as of
  Right ((l, vsl), (r, vsr)) -> 
    if r == 0 
      then toM vsl 
      else do
        hl <- toM vsl
        hr <- toM vsr
        return $ Branch (B.pack $ unpack $ hashWith SHA256 $ append (getHash hl) (getHash hr)) hl hr


powerSplit :: [a] -> Either Err ((Int, [a]), (Int, [a]))
powerSplit vs = do
  (l, r) <- powerSplitNum (L.length vs)
  let (vsl, vsr) = L.splitAt l vs
  return ((l, vsl), (r, vsr))

powerSplitNum :: Int -> Either Err (Int, Int)
powerSplitNum i = if i > 0 then Right (l, i - l) else Left "i <= 0" where
    l = head $ dropWhile (< i `div` 2) $ (\x -> 2 ^ x) <$> [0..]

test :: IO (Either [Char] (M B.ByteString B.ByteString, String))
test = runExceptT $ runWriterT $ toM $ d 1000

d n = B.pack <$> L.replicate n (fromIntegral <$> [1..10])