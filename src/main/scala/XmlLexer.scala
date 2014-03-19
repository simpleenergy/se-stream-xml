package com.simpleenergy.xml.stream

import scalaz.Catchable
import scalaz.Foldable
import scalaz.MonadState
import scalaz.Monoid
import scalaz.OptionT
import scalaz.State
import scalaz.StateT
import scalaz.std.list._
import scalaz.std.option._
import scalaz.stream
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._

sealed trait XmlToken
// TODO: entities, cdata, comments, qualified names
case class XmlTokenStart(name: String, attributes: List[XmlAttribute], empty: Boolean) extends XmlToken
case class XmlTokenEnd(name: String) extends XmlToken
case class XmlTokenText(content: String) extends XmlToken

/**
  * scalaz-stream based XML lexer
  *
  * Largely ported from Galois' Haskell xml package
  */
object XmlLexer {
  type TokenState[A] = OptionState[Array[Byte], A]

  object TokenState {
    def apply[A](s: Array[Byte] => OptionTrampoline[(Array[Byte], A)]): TokenState[A] = StateT(s)
  }

  val T = MonadState[OptionState, Array[Byte]]

  private [stream] def break[A](f: A => Boolean, xs: Array[A]): Option[(Array[A], Array[A])] = {
    val index = xs.indexWhere(f)
    if (index == -1)
      None
    else
      Some(xs.splitAt(index))
  }

  private[stream] def tailOption[A](a: Array[A]) =
    if (a.length > 0)
      Some(a.tail)
    else
      None

  def breakS(f: Byte => Boolean): TokenState[String] = TokenState { (c: Array[Byte]) =>
    trampolineOption(break(f, c).map {
      case (s, c) => (c, new String(s))
    })
  }

  def isSpace(c: Char) = java.lang.Character.isWhitespace(c)
  def endName(c: Char) = isSpace(c) || c == '=' || c == '>' || c == '/'

  val dropSpace: TokenState[Unit] = T.modify { (c: Array[Byte]) =>
    c.dropWhile((isSpace _).compose(_.toChar))
  }

  val qualName: TokenState[String] =
    breakS((endName _).compose(_.toChar))

  val skipByte: TokenState[Unit] =
    TokenState { (c: Array[Byte]) =>
      trampolineOption(tailOption(c).map((_, ())))
    }

  val stringVal: TokenState[String] =
    // "
    skipByte *>
      breakS(_ == '"') <*
      // "
      skipByte

  val attribVal: TokenState[String] =
    // =
    skipByte *>
      stringVal

  val attrib: TokenState[XmlAttribute] =
    (qualName |@| attribVal)(XmlAttribute)

  def attribs: TokenState[(List[XmlAttribute], Boolean)] = TokenState { (c: Array[Byte]) =>
    trampolineOption(c.headOption).flatMap {
      case '>' => (c.tail, (List.empty[XmlAttribute], false)).point[OptionTrampoline]
      // TODO: Assumption of "/>" or "?>"
      case '/' | '?' => trampolineOption(tailOption(c.tail).map { t => (t, (List.empty[XmlAttribute], true)) })
      case _ =>
        ((attrib |@| dropSpace *> attribs) { case (a, (as, b)) =>
          (a :: as, b)
        }).apply(c)
    }
  }

  val tag: TokenState[XmlToken] = TokenState { (c: Array[Byte]) =>
    trampolineOption(c.headOption).flatMap {
      case '/' =>
        ((qualName <* dropSpace <* T.modify { (c: Array[Byte]) =>
          c.headOption.fold(c) {
            case '>' =>
              c.tail
            case _ =>
              c
          }
        }).map[XmlToken](XmlTokenEnd)).apply(c.tail)
      case _ =>
        ((qualName |@| dropSpace *> attribs) { case (qn, (as, b)) =>
          XmlTokenStart(qn, as, b)
        }: TokenState[XmlToken])(c)
    }
  }

  def byteHead: TokenState[Byte] = TokenState { (c: Array[Byte]) =>
    trampolineOption(c.headOption.map((c, _)))
  }

  def tokens: State[Array[Byte], List[XmlToken]] = State { (c: Array[Byte]) =>
    def recurse: TokenState[List[XmlToken]] =
      dropSpace *> TokenState { (c: Array[Byte]) =>
        trampolineOption(c.headOption).flatMap {
          case '<' =>
            (dropSpace *> (tag |@| recurse) {
              _ :: _
            })(c.tail)
          case _ =>
            ((breakS(_ == '<').map(XmlTokenText) |@| recurse) {
              _ :: _
            }).apply(c)
        } orElse (c, List.empty[XmlToken]).point[OptionTrampoline]
      }

    (recurse(c) getOrElse ((Array.empty, Nil))).run
  }

  def lexString(string: String) = tokens(string.toArray.map(_.toByte))

  def lexFoldable[F[_]: Foldable](f: F[Array[Byte]]) =
    f.foldLeftM[({type l[a]=State[Array[Byte], a]})#l, List[XmlToken]](Nil) { case (accum, x) =>
      (State.modify((_: Array[Byte]) ++ x) *> tokens).map(accum ++ _)
    }.run(Array.empty)

  def lexProcess[F[_]](p: stream.Process[F, Array[Byte]]): stream.Process[F, List[XmlToken]] =
    processMonoidState(p, tokens)(Monoid.instance(_ ++ _, Array.empty))

  def lexFilename(filename: XmlFilename, chunkSize: Int) = {
    val input = stream.io.fileChunkR(filename.string)
    lexProcess(stream.Process.constant(chunkSize).through(input))
  }
}
