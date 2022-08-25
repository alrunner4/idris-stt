-- vim: cc=91 tw=90 shiftwidth=3
module Fibonacci
import Control.Monad.STT
import Control.Monad.Maybe
import Control.Monad.Trans

record FibCache (stt: STThread -> Type -> Type) (s: STThread) where
   constructor FibCacheSized
   {mst: MonadST stt s}
   cacheSize: Int
   cache: ArrayRef @{mst} s Integer

newFibCache: MonadST stt s => Int -> stt s (FibCache stt s)
newFibCache n = FibCacheSized n <$> newSTTArray n

(.fib): FibCache stt s -> Int -> stt s (Maybe Integer)
(FibCacheSized _ _).fib 0 = pure (Just 1)
(FibCacheSized _ _).fib 1 = pure (Just 1)
(FibCacheSized sz cache).fib n = if n < 0 || n >= sz
   then pure Nothing
   else do
      Nothing <- readSTTArray cache n
         | Just x => pure (Just x)
      runMaybeT$ do
         fib_n_sub_2 <- MkMaybeT$ (FibCacheSized sz cache).fib (n-2)
         fib_n_sub_1 <- MkMaybeT$ (FibCacheSized sz cache).fib (n-1)
         let fib_n = fib_n_sub_2 + fib_n_sub_1
         ignore$ lift$ writeSTTArray cache n fib_n
         pure fib_n

main: IO ()
main = runSTT {m = IO}$ do
   fc <- newFibCache 43
   printLn !(fc.fib 42)

