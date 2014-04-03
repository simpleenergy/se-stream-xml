package com.simpleenergy.xml.stream

import scalaz.@?>
import scalaz.MonadState
import scalaz.PLens
import scalaz.State
import scalaz.StateT
import scalaz.StoreT
import scalaz.std.string._
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.equal._

sealed trait XmlNode
case class XmlNodeElement(name: String, attribs: List[XmlAttribute], content: List[XmlNode]) extends XmlNode
case class XmlNodeText(content: String) extends XmlNode
object XmlNode {
  val elementNamePLens: XmlNode @?> String = PLens.plens {
    case e@XmlNodeElement(name, _, _) => Some(StoreT.store(name) { n => e.copy(name=n) })
    case XmlNodeText(_) => None
  }
  val elementAttribsPLens: XmlNode @?> List[XmlAttribute] = PLens.plens {
    case e@XmlNodeElement(_, attribs, _) => Some(StoreT.store(attribs) { a => e.copy(attribs=a) })
    case XmlNodeText(_) => None
  }
  val elementContentPLens: XmlNode @?> List[XmlNode] = PLens.plens {
    case e@XmlNodeElement(_, _, content) => Some(StoreT.store(content) { c => e.copy(content=c) })
    case XmlNodeText(_) => None
  }
  val textContentPLens: XmlNode @?> String = PLens.plens {
    case t@XmlNodeText(content) => Some(StoreT.store(content) { c => t.copy(content=c) })
    case XmlNodeElement(_, _, _) => None
  }
  def namedElementContentPLens(Name: String): XmlNode @?> List[XmlNode] = PLens.plens {
    case e@XmlNodeElement(Name, _, _) => elementContentPLens(e)
    case _ => None
  }
}

object XmlParser {
  type ParseState[A] = OptionState[List[XmlToken], A]

  val T = MonadState[OptionState, List[XmlToken]]

  object ParseState {
    def apply[A](s: List[XmlToken] => OptionTrampoline[(List[XmlToken], A)]): ParseState[A] = StateT(s)
  }

  // Close <-> Boolean
  sealed trait Close {
    def fold[A](found: A, notFound: => A): A = this match {
      case CloseFound => found
      case CloseNotFound => notFound
    }
  }
  object Close {
    val found: Close = CloseFound
    val notFound: Close = CloseNotFound
  }
  case object CloseFound extends Close
  case object CloseNotFound extends Close

  def parseTokens: State[List[XmlToken], List[XmlNode]] = State { (t: List[XmlToken]) =>
    def recurse: OptionState[(List[XmlToken], Close), List[XmlNode]] =
      StateT { case (t, b) =>
        trampolineOption(t.headOption).flatMap {
          // Ignore
          case o if o.isDeclaration =>
            recurse((t.tail, Close.notFound))
          case XmlTokenText(content) =>
            (recurse.map(XmlNodeText(content) :: _)).apply((t.tail, Close.notFound))
          case XmlTokenStart(name, attribs, true) =>
            (recurse.map(XmlNodeElement(name, attribs, Nil) :: _)).apply((t.tail, Close.notFound))
          case XmlTokenStart(name, attribs, false) =>
            for {
              inner <- recurse((t.tail, Close.notFound))
              ((innerTail, closed), innerNodes) = inner
              // We didn't find a close? Let's not process these tokens until we do.
              _ <- closed.fold(
                ().point[OptionTrampoline],
                trampolineOption(None)
              )
              outer <- recurse((innerTail, Close.notFound))
              (outerState, outerNodes) = outer
            } yield (outerState, XmlNodeElement(name, attribs, innerNodes) :: outerNodes)
          // TODO: Assumption, tokens are always balanced
          case XmlTokenEnd(_) =>
            ((t.tail, Close.found), List.empty[XmlNode]).point[OptionTrampoline]
        } orElse ((t, b), List.empty[XmlNode]).point[OptionTrampoline]
      }

    (recurse((t, Close.notFound)) getOrElse (((List.empty, Close.notFound), Nil))).run match {
      case ((tokens, _), nodes) => (tokens, nodes)
    }
  }

  // TODO: Ignores any left over characters
  def parseString(string: String) = parseTokens(XmlLexer.tokens(string)._2)
}
