module Network.Server.Chat.Loop where

import Prelude hiding (mapM_, catch)
import Network(PortID(..), sClose, withSocketsDo, listenOn)
import System.IO(BufferMode(..))
import Data.IORef(IORef, newIORef, readIORef)
import Data.Foldable(Foldable, mapM_)
import Control.Applicative(Applicative, pure, (<*>))
import Control.Concurrent(forkIO)
import Control.Exception(finally, try, catch, Exception)
import Control.Monad(forever)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))

import Network.Server.Common.Accept
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Env
import Network.Server.Common.Ref
import Data.Set(Set)
import qualified Data.Set as S

data Loop v f a =
  Loop (Env v -> f a)

type IOLoop v a =
  Loop v IO a

type IORefLoop v a =
  IOLoop (IORef v) a

instance Functor f => Functor (Loop v f) where
  -- fmap :: (a -> b) -> Loop v f a -> Loop v f b
  fmap f (Loop k) =
    Loop (fmap f . k)

instance Applicative f => Applicative (Loop v f) where
  pure =
    Loop . pure . pure
  Loop f <*> Loop x =
    Loop (\a -> f a <*> x a)

instance Monad f => Monad (Loop v f) where
  -- left return probably refers to Reader monad
  -- right return probably reverse to Monad f
  return =
    Loop . return . return
  -- (>>=) :: Loop v f a -> (a -> Loop v f b) -> Loop v f b
  Loop k >>= f =
    Loop (\v -> k v >>= \a ->
      let Loop l = f a
      in l v)

instance MonadTrans (Loop v) where
  -- lift :: (Monad m, MonadTrans t) => m a -> t m a
  -- Loop :: (Env v -> s -> f (a, s)) -> Loop v s f a
  -- const :: a -> b -> a
  -- Loop . const :: (s -> f (a, s)) -> Loop v s f a
  lift = Loop . const

instance MonadIO f => MonadIO (Loop v f) where
  liftIO =
    lift . liftIO

-- exception try?
etry ::
  Exception e =>
  (Env v -> IO a)
  -> IOLoop v (Either e a)
etry k =
  Loop $ try . k

server ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v () -- per-client
  -> IO a
server i r (Loop f) =
  let hand s w c = forever $
                     do q <- accept' s
                        _ <- lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- r w
                        forkIO (f (Env q c x))
  -- https://hackage.haskell.org/package/network-2.6.3.1/docs/Network.html
  in withSocketsDo $ do 
       s <- listenOn (PortNumber 6060)
       w <- i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

-- https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/base-4.9.0.0/Data-IORef.html
allClients :: IOLoop v (Set Ref)
allClients = Loop $ \env -> readIORef (getL clientsL env)


-- think of this as
-- IOLoop v () -> (String -> IOLoop v ()) -> IOLoop v ()

-- <Gurkenglas> peterbecich, we only have one v to pass to all places that
--  require it, so the type signature may as well be:
--  ((Accept, IORef (Set Ref)) -> IO ()) ->
--    (String -> (Accept, IORef (Set Ref)) -> IO ()) ->
--    (Accept, IORef (Set Ref)) ->
--    IO ()	        [12:50]


-- <Gurkenglas> I'll just guess that we'll hardly be expected to build new IORefs
-- 	     here instead of passing the one already have, but we might well
-- 	     want to access it, so:
-- IORef (Set Ref) ->
--   (Accept -> IO ()) ->
--   (String -> Accept -> IO ()) ->
--   Accept ->
--   IO ()	        [12:51]

-- perClient ::
--   Loop v IO x
--   -> (String -> Loop v IO a)
--   -> Loop v IO ()

-- perClient ::
--   ((Accept, IORef (Set Ref)) -> IO ()) ->
--   (String -> ((Accept, IORef (Set Ref)) -> IO ())) ->
--   ((Accept, IORef (Set Ref)) -> IO ())

-- (>>=) :: Loop v f a -> (a -> Loop v f b) -> Loop v f b

perClient ::
  IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v a) -- read line from client
  -> IOLoop v ()
perClient loop func = do
  env <- readEnv
  let acc = getL acceptL env
  undefined
  
  -- Loop $ \env ->
  -- let acc = getL acceptL env --
  --     fromClient = func (show acc)
  -- in putStrLn (show acc)


  -- do
  -- clientAccepted <- loop
  -- others <- allClients
  

  -- do
  -- others <- allClients
  -- pPutStrLn $ show others
  


  -- Loop $ \_ -> putStrLn "foo"


loop ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v w) -- read line from client
  -> IO a
loop i r q f =
  server i r (perClient q f)

iorefServer ::
  v -- server initialise
  -> IORefLoop v () -- per-client
  -> IO a
iorefServer x =
  server (newIORef x) return

iorefLoop ::
  v -- server initialise
  -> IORefLoop v x -- client accepted (post)
  -> (String -> IORefLoop v w) -- read line from client
  -> IO a
iorefLoop x q f =
  iorefServer x (perClient q f)

pPutStrLn ::
  String
  -> IOLoop v ()
pPutStrLn s =
  Loop (\handle -> lPutStrLn handle s)

(!) ::
  Foldable t =>
  IOLoop v (t Ref)
  -> String
  -> IOLoop v ()
clients ! msg =
 clients >>= purgeClients (\y -> liftIO (lPutStrLn y msg))

infixl 2 !

purgeClients ::
  Foldable t =>
  (Ref -> IOLoop v ())
  -> t Ref
  -> IOLoop v ()
purgeClients a =
  mapM_ (\y ->
    ecatch (a y)
      (\x -> do _ <- modifyClients (S.delete y)
                xprint x)
        )

readEnv ::
  Applicative f =>
  Loop v f (Env v)
readEnv =
  Loop $ pure

readEnvval ::
  Applicative f =>
  Loop v f v
readEnvval =
  fmap (getL envvalL) readEnv

readIOEnvval ::
  IORefLoop a a
readIOEnvval =
  Loop $ \env ->
    readIORef (getL envvalL env)

allClientsButThis ::
  IOLoop v (Set Ref)
allClientsButThis =
  Loop $ \env ->
    fmap (S.delete ((acceptL .@ refL) `getL` env)) (readIORef (getL clientsL env))

-- Control.Monad.CatchIO
ecatch ::
  Exception e =>
  IOLoop v a
  -> (e -> IOLoop v a)
  -> IOLoop v a
ecatch (Loop k) f =
  Loop $ \env -> k env `catch` (\e -> let Loop l = f e in l env)

modifyClients ::
  (Set Ref -> Set Ref)
  -> IOLoop v (Set Ref)
modifyClients f =
  Loop $ \env ->
    atomicModifyIORef_ (clientsL `getL` env) f
