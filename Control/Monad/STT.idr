-- vim: tw=90 cc=91 shiftwidth=3
module Control.Monad.STT
import Data.IOArray
import Data.IORef

import Data.IOArray
import Data.IORef

%hide Prelude.Types.elem

%default total

export STThread: Type; STThread = Type

||| A mutable reference, bound to a state thread.
|||
||| A value of type `STRef s a` contains a mutable `a`, bound to `STThread` `s`. Any
||| access to the reference must occur in an `ST s` monad with the same thread.
export
data STRef: STThread -> Type -> Type where
   MkSTRef: IORef a -> STRef s a

||| An array of mutable references, bound to a state thread.
export
data STArray: STThread -> Type -> Type where
   MkSTArray: IOArray a -> STArray s a

||| The `ST` monad allows for mutable access to references, but unlike `IO`, it is safely
||| "escapable".
|||
||| The parameter `s` is an opaque "thread variable" that ensures mutable references
||| cannot be shared between threads. 
export
data ST: STThread -> Type -> Type where
   MkST: IO a -> ST s a

||| Run an `ST` computation.
export
runST: (forall s. ST s a) -> a
runST p = 
   let
      s = ()
      MkST prog = p {s}
   in
      unsafePerformIO prog

export
Functor (ST s) where
  map fn (MkST st) = MkST $ fn <$> st

export
Applicative (ST s) where
  pure = MkST . pure
  MkST f <*> MkST a = MkST $ f <*> a

export
Monad (ST s) where
  MkST p >>= k
      = MkST $ do p' <- p
                  let MkST kp = k p'
                  kp

||| Create a new mutable reference with the given value.
export
newSTRef: a -> ST s (STRef s a)
newSTRef val
    = MkST $ do r <- newIORef val
                pure (MkSTRef r)

export
newSTArray: Int -> ST s (STArray s a)
newSTArray size
    = MkST $ do r <- newArray size
                pure (MkSTArray r)

||| Read the value of a mutable reference.
|||
||| This occurs within `ST s` to prevent `STRef`s from being usable if they are
||| "leaked" via `runST`.
%inline
export
readSTRef: STRef s a -> ST s a
readSTRef (MkSTRef r) = MkST $ readIORef r

||| Write to a mutable reference.
%inline
export
writeSTRef: STRef s a -> (val : a) -> ST s ()
writeSTRef (MkSTRef r) val = MkST $ writeIORef r val

||| Apply a function to the contents of a mutable reference.
export
modifySTRef: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f
    = do val <- readSTRef ref
         writeSTRef ref (f val)

%inline
export
readSTArray: STArray s a -> Int -> ST s (Maybe a)
readSTArray (MkSTArray r) i = MkST $ readArray r i

%inline
export
writeSTArray: STArray s a -> Int -> a -> ST s Bool
writeSTArray (MkSTArray r) i x = MkST $ writeArray r i x

export
modifySTArray: STArray s a -> Int -> (Maybe a -> a) -> ST s Bool
modifySTArray arr i f
    = do val <- readSTArray arr i
         writeSTArray arr i (f val)

||| The ST monad transformer.
|||
||| `STT s m a` is an operation on the state thread `s` in `Monad m` with result `a`.
export
data STT: (Type -> Type) -> STThread -> Type -> Type where
   MkSTT: IO a -> (a -> m b) -> STT m s b

||| `STTRef s a` is the type of mutable references in state thread `s`.
export
data STTRef: STThread -> Type -> Type where
   MkSTTRef: IORef a -> STTRef s a

||| `STTArray s a` is the type of mutable array references in state thread `s`.
export
data STTArray: STThread -> Type -> Type where
   MkSTTArray: IOArray a -> STTArray s a

||| `runSTT op` creates a state thread and uses it to evaluate the mutable computation
||| `op` which may have actions in `Monad m` in addition to `MonadST`.
export runSTT: ({s: _} -> STT m s a) -> m a
runSTT p = let MkSTT st act = p {s = ()} in act (unsafePerformIO st)

public export
interface Monad (m s) => MonadST (0 m: STThread -> Type -> Type) (0 s: STThread) where
   constructor MkMonadST
   VarRef: STThread -> Type -> Type
   ArrayRef: STThread -> Type -> Type
   newSTTRef: a -> m s (VarRef s a)
   readSTTRef:  VarRef s a -> m s a
   writeSTTRef: VarRef s a -> a -> m s ()
   newSTTArray: Int -> m s (ArrayRef s a)
   readSTTArray:  ArrayRef s a -> Int -> m s (Maybe a)
   writeSTTArray: ArrayRef s a -> Int -> a -> m s Bool

export
Functor m => Functor (STT m s) where
   map f (MkSTT st m_a) = MkSTT st (\x => map f (m_a x))

export
Applicative m => Applicative (STT m s) where
   pure x = MkSTT (pure ()) (const$ pure x)
   MkSTT st1 f <*> MkSTT st2 x = MkSTT (pure ()) $const$
      f (unsafePerformIO st1) <*> x (unsafePerformIO st2)

export
Monad m => Monad (STT m s) where
   MkSTT stt1 act >>= f = MkSTT stt1 $ \u1 => do
      r1 <- act u1
      let MkSTT stt2 act2 = f r1
      act2 (unsafePerformIO stt2)

export
Monad m => MonadST (STT m) s where
   VarRef = STTRef
   ArrayRef = STTArray
   newSTTRef = \x => MkSTT (newIORef x) (pure . MkSTTRef)
   readSTTRef = \(MkSTTRef r)=> MkSTT (readIORef r) pure
   writeSTTRef = \(MkSTTRef r), x => MkSTT (writeIORef r x) pure
   newSTTArray = \size => MkSTT (newArray size) (pure . MkSTTArray)
   readSTTArray = \(MkSTTArray r), i => MkSTT (readArray r i) pure
   writeSTTArray = \(MkSTTArray r), i, x => MkSTT (writeArray r i x) pure

export
HasIO m => HasIO (STT m s) where
   liftIO a = MkSTT (pure ()) (const (liftIO a))
