package com.simpleenergy.xml

import scalaz.Free.Trampoline
import scalaz.Monoid
import scalaz.OptionT
import scalaz.State
import scalaz.StateT
import scalaz.Trampoline
import scalaz.std.list._
import scalaz.stream.Process
import scalaz.stream.Process.{Await, Emit, Halt}
import scalaz.syntax.foldable._

package stream {
  case class XmlFilename(string: String)
  case class XmlAttribute(key: String, value: String)
}

package object stream {
  // type aliases cause implicit resolution to work
  type OptionTrampoline[A] = OptionT[Trampoline, A]
  type OptionState[S, A] = StateT[OptionTrampoline, S, A]

  private [stream] def trampolineOption[A](o: Option[A]): OptionTrampoline[A] =
    OptionT(Trampoline.done(o))

  // TODO: Move this to scalaz.stream
  def processMonoidState[F[_], M: Monoid, A](p: Process[F, M], s: State[M, A]): Process[F, A] = {
    def recurse(q: Process[F, M], c: M): Process[F, A] =
      q match {
        case h@Halt(_) => h
        // Scala can't figure out the type of Process#next in this pattern match
        // Surprisingly doesn't trigger the erasure warning either
        case Emit(h: Seq[M], t: Process[F, M]) =>
          val combined = (c :: h.toList).suml
          val (d, result) = s(combined)
          Process.emit(result) ++ recurse(t, d)
        case Await(req, recv, fb: Process[F, M], d: Process[F, M]) =>
          // TODO: scalaz-stream has AwaitF#unapply - a safer upcast to Any, but as a private...
          Process.await[F, Any, A](req.asInstanceOf[F[Any]])(
            (a: Any) => recurse(recv.asInstanceOf[Any => Process[F, M]](a), c),
            recurse(fb, c),
            recurse(d, c)
          )
      }

    recurse(p, Monoid[M].zero)
  }
}
