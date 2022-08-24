# package stt

## Extending base's ST with IOArrays

The `Control.Monad.STT.ST` type is otherwise the same as base's `Control.Monad.ST.ST`, but rather
than sequencing only operations on single-valued `STRef`s, `package stt` provides `STTRef` and
`STTArray` for safe sequencing of conceptually-pure computations with `IORef` and `IOArray`.

## The STT Monad Transformer

Since `ST` is a conceptually pure monad, we can extend other monads with mutable state just like we
can with the traditional pure `Control.Monad.State.StateT` with the new interface
`Control.Monad.STT.MonadSTT` and its canonical transformer `Control.Monad.STT.STT`.

