{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StaticLS.App
  (
  -- App,
  --   -- HasIO (..),
  -- IOEnv (..),
  )
where

import Control.Monad.Reader
import Data.Functor.Identity
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Rec (Rec)
import Data.Rec qualified as Rec

newtype App env a = UnsafeApp {unApp :: ReaderT (Rec Identity env) IO a}
  deriving (Functor, Applicative, Monad)

runHas :: x -> App (Uses x env) a -> App env a
runHas x (UnsafeApp m) = UnsafeApp $ ReaderT \env -> runReaderT m (Rec.cons (Identity x) env)

has :: (Has x env) => App env x
has = UnsafeApp $ ReaderT \env -> pure $ runIdentity $ Rec.index env

unsafeLiftIO :: IO a -> App env a
unsafeLiftIO = UnsafeApp . liftIO

runState :: s -> App (State s : env) a -> App env a
runState s m = do
  ref <- unsafeLiftIO $ IORef.newIORef s
  runHas (State ref) m

get :: (Has (State s) env) => App env s
get = do
  State ref <- has
  unsafeLiftIO $ IORef.readIORef ref

put :: (Has (State s) env) => s -> App env ()
put s = do
  State ref <- has
  unsafeLiftIO $ IORef.writeIORef ref s

runReader :: r -> App (r : env) a -> App env a
runReader r m = runHas r m

data MyEffectInternalState = MyEffectInternalState

data GetInternal x env where
  GetInternal :: (Has x env) => GetInternal x env

type Internal x = forall env. GetInternal x env

data MyEffect = MyEffect
  { internalState :: Internal (State MyEffectInternalState)
  }

runMyEffect :: (Has (State MyEffectInternalState) env) => App (Uses MyEffect env) a -> App env a
runMyEffect = runHas MyEffect {internalState = GetInternal}

performMyEffect :: forall env. (Has MyEffect env) => App env ()
performMyEffect = do
  MyEffect {internalState = internal} <- has
  case internal @env of
    GetInternal -> do
      MyEffectInternalState <- get
      put MyEffectInternalState
      pure ()

data IOEnv = IOEnv

data State s = State !(IORef s)

type Has = Rec.Elem

type Uses = (:)

-- runHas :: x -> App (Uses x env) a -> App env a
-- runHas x (UnsafeApp m) = UnsafeApp $ ReaderT \env -> runReaderT m (Rec.cons (Identity x) env)

-- has :: (Has x env) => App env x
-- has = UnsafeApp $ ReaderT \env -> pure $ runIdentity $ Rec.index env

-- unsafeLiftIO :: IO a -> App env a
-- unsafeLiftIO = UnsafeApp . liftIO

-- runState :: s -> App (State s : env) a -> App env a
-- runState s m = do
--   ref <- unsafeLiftIO $ IORef.newIORef s
--   runHas (State ref) m

-- get :: (Has (State s) env) => App env s
-- get = do
--   State ref <- has
--   unsafeLiftIO $ IORef.readIORef ref

-- put :: (Has (State s) env) => s -> App env ()
-- put s = do
--   State ref <- has
--   unsafeLiftIO $ IORef.writeIORef ref s

-- runReader :: r -> App (r : env) a -> App env a
-- runReader r m = runHas r m

-- data MyEffectInternalState = MyEffectInternalState

-- data GetInternal x env where
--   GetInternal :: (Has x env) => GetInternal x env

-- type Internal x = forall env. GetInternal x env

-- data MyEffect = MyEffect
--   { internalState :: Internal (State MyEffectInternalState)
--   }

-- runMyEffect :: (Has (State MyEffectInternalState) env) => App (Uses MyEffect env) a -> App env a
-- runMyEffect = runHas MyEffect {internalState = GetInternal}

-- performMyEffect :: forall env. (Has MyEffect env) => App env ()
-- performMyEffect = do
--   MyEffect {internalState = internal} <- has
--   case internal @env of
--     GetInternal -> do
--       MyEffectInternalState <- get
--       put MyEffectInternalState
--       pure ()
