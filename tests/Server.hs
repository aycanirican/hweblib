
--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.ByteString.Char8      (pack)
import           Network.Socket
import           Network.Http
--------------------------------------------------------------------------------

main :: IO ()
main = do
  addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing
                 (Just "8888")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  setSocketOption sock NoDelay 1
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  forever $ do
    (conn, _) <- accept sock
    let r = pack <$> (putStrLn "receiving" >> recv conn 4096 )
    initial <- r
    req <- parseWith r parseMessage initial
    print req
    close conn






