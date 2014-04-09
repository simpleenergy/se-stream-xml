package com.simpleenergy.xml.stream

import java.lang.String
import scala.collection.immutable.StringOps
import scala.{Boolean, Char, List, Nil, None, Some, Option, Unit}

import scalaz.Catchable
import scalaz.Foldable
import scalaz.MonadState
import scalaz.Monoid
import scalaz.OptionT
import scalaz.State
import scalaz.StateT
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.stream
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._

sealed trait XmlToken {
  def isDeclaration: Boolean = this match {
    case XmlTokenStart("?xml", _, true) => true
    case _ => false
  }
}
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
  type TokenState[A] = OptionState[String, A]

  object TokenState {
    def apply[A](s: String => OptionTrampoline[(String, A)]): TokenState[A] = StateT(s)
  }

  val T = MonadState[OptionState, String]

  private [stream] def break(f: Char => Boolean, xs: String): Option[(String, String)] = {
    val index = new StringOps(xs).indexWhere(f)
    if (index == -1)
      None
    else
      Some(new StringOps(xs).splitAt(index))
  }

  private[stream] def tailOption(a: String) =
    if (a.length > 0)
      Some(new StringOps(a).tail)
    else
      None

  def breakS(f: Char => Boolean): TokenState[String] = TokenState { (c: String) =>
    trampolineOption(break(f, c).map {
      case (s, c) => (c, new String(s))
    })
  }

  def isSpace(c: Char) = java.lang.Character.isWhitespace(c)
  def endName(c: Char) = isSpace(c) || c == '=' || c == '>' || c == '/'

  val dropSpace: TokenState[Unit] = T.modify(new StringOps(_).dropWhile(XmlLexer.isSpace))

  val qualName: TokenState[String] =
    breakS(endName)

  val skipChar: TokenState[Unit] =
    TokenState { (c: String) =>
      trampolineOption(tailOption(c).map((_, ())))
    }

  val stringVal: TokenState[String] =
    // "
    skipChar *>
      breakS(_ == '"') <*
      // "
      skipChar

  val attribVal: TokenState[String] =
    // =
    skipChar *>
      stringVal

  val attrib: TokenState[XmlAttribute] =
    (qualName |@| attribVal)(XmlAttribute)

  def attribs: TokenState[(List[XmlAttribute], Boolean)] = TokenState { (c: String) =>
    trampolineOption(new StringOps(c).headOption).flatMap {
      case '>' => (new StringOps(c).tail, (List.empty[XmlAttribute], false)).point[OptionTrampoline]
      // TODO: Assumption of "/>" or "?>"
      case '/' | '?' => trampolineOption(tailOption(new StringOps(c).tail).map { t => (t, (List.empty[XmlAttribute], true)) })
      case _ =>
        ((attrib |@| dropSpace *> attribs) { case (a, (as, b)) =>
          (a :: as, b)
        }).apply(c)
    }
  }

  val tag: TokenState[XmlToken] = TokenState { (c: String) =>
    trampolineOption(new StringOps(c).headOption).flatMap {
      case '/' =>
        ((qualName <* dropSpace <* T.modify { (c: String) =>
          new StringOps(c).headOption.fold(c) {
            case '>' =>
              new StringOps(c).tail
            case _ =>
              c
          }
        }).map[XmlToken](XmlTokenEnd)).apply(new StringOps(c).tail)
      case _ =>
        ((qualName |@| dropSpace *> attribs) { case (qn, (as, b)) =>
          XmlTokenStart(qn, as, b)
        }: TokenState[XmlToken])(c)
    }
  }

  def tokens: State[String, List[XmlToken]] = State { (c: String) =>
    def recurse: TokenState[List[XmlToken]] =
      dropSpace *> TokenState { (c: String) =>
        trampolineOption(new StringOps(c).headOption).flatMap {
          case '<' =>
            (dropSpace *> (tag |@| recurse) {
              _ :: _
            })(new StringOps(c).tail)
          case _ =>
            ((breakS(_ == '<').map(XmlTokenText) |@| recurse) {
              _ :: _
            }).apply(c)
        } orElse (c, List.empty[XmlToken]).point[OptionTrampoline]
      }

    (recurse(c) getOrElse (("", Nil))).run
  }

  def lexFoldable[F[_]: Foldable](f: F[String]) =
    f.foldLeftM[({type l[a]=State[String, a]})#l, List[XmlToken]](Nil) { case (accum, x) =>
      (State.modify((_: String) + x) *> tokens).map(accum ++ _)
    }.run("")

  def lexProcess[F[_]](p: stream.Process[F, String]): stream.Process[F, List[XmlToken]] =
    processMonoidState(p, tokens)

  def lexFilename(filename: XmlFilename) = {
    val input = stream.io.linesR(filename.string)
    lexProcess(input)
  }
}
