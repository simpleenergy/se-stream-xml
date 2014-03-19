package com.simpleenergy.xml.stream

import scalaz.MonadState
import scalaz.State
import scalaz.StateT
import scalaz.std.string._
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.equal._

sealed trait XmlNode
case class XmlNodeElement(name: String, attribs: List[XmlAttribute], content: List[XmlNode]) extends XmlNode
case class XmlNodeText(content: String) extends XmlNode

object XmlParser {
  type ParseState[A] = OptionState[List[XmlToken], A]

  val T = MonadState[OptionState, List[XmlToken]]

  object ParseState {
    def apply[A](s: List[XmlToken] => OptionTrampoline[(List[XmlToken], A)]): ParseState[A] = StateT(s)
  }

  def parseTokens: State[List[XmlToken], List[XmlNode]] = State { (t: List[XmlToken]) =>
    def recurse: ParseState[List[XmlNode]] =
      ParseState { (t: List[XmlToken]) =>
        trampolineOption(t.headOption).flatMap {
          // Ignore the XML header
          case XmlTokenStart("?xml", _, true) =>
            recurse(t.tail)
          case XmlTokenText(content) =>
            (recurse.map(XmlNodeText(content) :: _)).apply(t.tail)
          case XmlTokenStart(name, attribs, true) =>
            (recurse.map(XmlNodeElement(name, attribs, Nil) :: _)).apply(t.tail)
          case XmlTokenStart(name, attribs, false) =>
            for {
              inner <- recurse.apply(t.tail)
              (innerTail, innerNodes) = inner
              outer <- recurse.apply(innerTail)
              (outerTail, outerNodes) = outer
            } yield (outerTail, XmlNodeElement(name, attribs, innerNodes) :: outerNodes)
          // TODO: Assumption, tokens are always balanced
          case XmlTokenEnd(_) =>
            (t.tail, List.empty[XmlNode]).point[OptionTrampoline]
        } orElse (t, List.empty[XmlNode]).point[OptionTrampoline]
      }

    (recurse(t) getOrElse ((List.empty, Nil))).run
  }

  // TODO: Ignores any left over characters
  def parseString(string: String) = parseTokens(XmlLexer.lexString(string)._2)
}
