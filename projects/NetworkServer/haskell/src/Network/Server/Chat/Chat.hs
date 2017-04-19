module Network.Server.Chat.Chat where

import Network.Server.Common.Line
import Network.Server.Common.HandleLens (lPutStrLn)
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Data.IORef(atomicModifyIORef, readIORef)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))
import Control.Monad (replicateM_)
import Text.Read (readMaybe)

-- type Chat a =   Loop (IORef Integer) (IO a)
-- type Chat a = IOLoop (IORef Integer) a
type Chat a = IORefLoop Integer a

data ChatCommand =
  Chat String
  | Incr
  | Add Int
  | Unknown String
  deriving (Eq, Show)

incr :: Chat Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef e (\n -> (n + 1, n + 1))

chat :: IO a
chat =
  iorefLoop 0 (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
-- >>> chatCommand "foo"
-- Unknown "foo"
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"

-- trimPrefixThen :: String -> String -> Maybe String
chatCommand ::
  String
  -> ChatCommand
chatCommand z =
  Unknown z `fromMaybe` msum [
                               Chat <$> trimPrefixThen "CHAT" z
                             , Incr <$ trimPrefixThen "INCR" z
                             , do
                                 trimmed <- trimPrefixThen "ADD" z
                                 parsed <- readMaybe trimmed
                                 return $ Add parsed
                             ]

process :: ChatCommand -> Chat ()
-- improve this by printing result of increment in chat
-- mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
process Incr = incr >>= (pPutStrLn . show)
process (Unknown str) = pPutStrLn $ "Invalid: "++str
process (Chat str) = do
  -- otherClients :: Set Ref
  otherClients <- allClientsButThis
  mapM_ (\ref -> Loop (\_ -> lPutStrLn ref str)) otherClients
process (Add n) = do
  _ <- replicateM_ n incr
  e <- readEnvval
  i <- liftIO $ readIORef e
  pPutStrLn $ show i
