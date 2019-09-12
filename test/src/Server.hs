module Server where

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Merkle

type HashAPI = "proof" :> Capture "hash" Int :> Get '[JSON] String
  :<|> "ping" :> Get '[JSON] String

hashAPI :: Proxy HashAPI
hashAPI = Proxy

hashApp :: Server HashAPI
hashApp = hashProofApp :<|> pingApp

pingApp = return "pong"
hashProofApp :: Int -> Handler String
hashProofApp s = return $ test3 s

app :: IO ()
app = run 8888 (serve hashAPI hashApp)
