module TestSTT
import Control.Monad.STT
import Data.List

prompt: HasIO m => String -> m String
prompt text = putStrLn text >> putStr " > " >> getLine

run_stt_in: (m: Type -> Type) -> ({s: _} -> STT m s a) -> m a
run_stt_in m = runSTT {m}

main: IO ()
main = run_stt_in IO $ do
   name <- prompt "Who goes there?"
   x <- newSTTRef name
   num <- prompt "Oughtn't there be a number here, too?"
   let y = cast num
   for_ [1..y]$ \n => do
      putStr "Loop \{show n}\n"
      i <- readSTTRef x
      putStr "Replicating \{show n} \{i}.\n"
      writeSTTRef x $fastConcat$ "zap" :: replicate n i
   putStrLn !(readSTTRef x)
