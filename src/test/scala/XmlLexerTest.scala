package com.simpleenergy.xml.stream

import org.specs2.mutable._
import org.specs2.ScalaCheck

import scalaz.Catchable
import scalaz.Monad
import scalaz.\/
import scalaz.std.list._
import scalaz.std.tuple._
import scalaz.stream
import scalaz.syntax.bifunctor._

sealed trait SafeCatchable[A] {
  def toOption: Option[A] = this match {
    case SafeSome(a) => Some(a)
    case SafeNone() => None
  }
}
case class SafeSome[A](a: A) extends SafeCatchable[A]
case class SafeNone[A]() extends SafeCatchable[A]

object SafeCatchable {
  implicit object SafeCatchableInstances extends Catchable[SafeCatchable] with Monad[SafeCatchable] {
    def point[A](a: => A): SafeCatchable[A] = SafeSome(a)
    def bind[A, B](fa: SafeCatchable[A])(f: A => SafeCatchable[B]): SafeCatchable[B] = fa match {
      case SafeSome(a) => f(a)
      case SafeNone() => SafeNone()
    }
    def attempt[A](f: SafeCatchable[A]): SafeCatchable[scalaz.\/[Throwable,A]] = map(f)(\/.right)
    def fail[A](err: Throwable): SafeCatchable[A] = SafeNone()
  }
}

class XmlLexerTest extends Specification with ScalaCheck {
  "XML lexer" should {
    "tokenize a full document" in {
      val xml = """<?xml version="1.0"?><test a="b"><hello /></test>"""

      val expectedTokens =
        List(
          XmlTokenStart("?xml", XmlAttribute("version", "1.0") :: Nil, true),
          XmlTokenStart("test", XmlAttribute("a", "b") :: Nil, false),
          XmlTokenStart("hello", Nil, true),
          XmlTokenEnd("test")
        )

      XmlLexer.lexString(xml).leftMap(new String(_)) === (("", expectedTokens))
    }
    "tokenize a partial document" in {
      val xml = """<test a="b"><hello wo"""

      val expectedTokens =
        List(
          XmlTokenStart("test", XmlAttribute("a", "b") :: Nil, false)
        )

      XmlLexer.lexString(xml).leftMap(new String(_)) === (("<hello wo", expectedTokens))
    }
    "tokenize a group of partial documents" in {
      val xml = List(
        """<test a="b"><hello wo""",
        """rld="fantasy" />""",
        """</test>"""
      )

      val expectedTokens =
        List(
          XmlTokenStart("test", XmlAttribute("a", "b") :: Nil, false),
          XmlTokenStart("hello", XmlAttribute("world", "fantasy") :: Nil, true),
          XmlTokenEnd("test")
        )

      XmlLexer.lexFoldable(xml.map(_.toArray.map(_.toByte))).leftMap(new String(_)) === (("", expectedTokens))
    }
    "tokenize a streaming process" in {
      val p = stream.Process.emitAll(List("<ab", "c>", "hel", "lo", "</", "abc>").map(_.toArray.map(_.toByte)))
      val r: stream.Process[SafeCatchable, List[XmlToken]] = XmlLexer.lexProcess(p)

      val expectedTokens =
        List(
          XmlTokenStart("abc", Nil, false),
          XmlTokenText("hello"),
          XmlTokenEnd("abc")
        )

      r.runLog.toOption must be some(Vector(expectedTokens))
    }
  }

  "break" should {
    "be none after running until the end" in check { (f: Int => Boolean, xs: Array[Int]) =>
      !xs.exists(f) ==> {
        XmlLexer.break(f, xs) must beNone
      }
    }
    "retain more information than Array#span" in check { (f: Int => Boolean, xs: Array[Int]) =>
      xs.exists(f) ==> {
        // The arrays are not comparable, let's just use length
        def bilength[A](t: (Array[A], Array[A])) = t.bimap(_.length, _.length)
        XmlLexer.break(f, xs).map(bilength) === Some(bilength(xs.span(!f(_))))
      }
    }
  }
}
