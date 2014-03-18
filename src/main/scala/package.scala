package com.simpleenergy.xml

import scalaz.Free.Trampoline
import scalaz.OptionT
import scalaz.StateT
import scalaz.Trampoline

package object stream {
  // type aliases cause implicit resolution to work
  type OptionTrampoline[A] = OptionT[Trampoline, A]
  type OptionState[S, A] = StateT[OptionTrampoline, S, A]

  private [stream] def trampolineOption[A](o: Option[A]): OptionTrampoline[A] =
    OptionT(Trampoline.done(o))
}
