package com.simpleenergy.xml.stream

import org.specs2.mutable._

class XmlParserTest extends Specification {
  "XML parser" should {
    "parse a full string document" in {
      val xml = """<?xml version="1.0"?><hello a="b"><test /></hello>"""

      val expectedNodes =
        List(
          XmlNodeElement("hello", XmlAttribute("a", "b") :: Nil, List(
            XmlNodeElement("test", Nil, Nil)
          ))
        )

      XmlParser.parseString(xml) === ((Nil, expectedNodes))
    }
    "parse a full tokenized document" in {
      val inputTokens =
        List(
          XmlTokenStart("?xml", XmlAttribute("version", "1.0") :: Nil, true),
          XmlTokenStart("test", XmlAttribute("a", "b") :: Nil, false),
          XmlTokenStart("test", XmlAttribute("b", "a") :: Nil, false),
          XmlTokenStart("hello", Nil, true),
          XmlTokenEnd("test"),
          XmlTokenEnd("test"),
          XmlTokenStart("hello", Nil, true)
        )

      val expectedNodes =
        List(
          XmlNodeElement("test", XmlAttribute("a", "b") :: Nil, List(
            XmlNodeElement("test", XmlAttribute("b", "a") :: Nil, List(
              XmlNodeElement("hello", Nil, Nil)
            ))
          )),
          XmlNodeElement("hello", Nil, Nil)
        )

      XmlParser.parseTokens(inputTokens) === ((Nil, expectedNodes))
    }
    "parse a partially tokenized document" in {
      val inputTokens =
        List(
          XmlTokenStart("?xml", XmlAttribute("version", "1.0") :: Nil, true),
          XmlTokenStart("hello", Nil, true),
          XmlTokenStart("test", XmlAttribute("a", "b") :: Nil, false),
          XmlTokenStart("test", XmlAttribute("b", "a") :: Nil, false)
        )

      val expectedUnparsed =
        List(
          XmlTokenStart("test", XmlAttribute("a", "b") :: Nil, false),
          XmlTokenStart("test", XmlAttribute("b", "a") :: Nil, false)
        )

      val expectedNodes =
        List(
          XmlNodeElement("hello", Nil, Nil)
        )

      XmlParser.parseTokens(inputTokens) === ((expectedUnparsed, expectedNodes))
    }
  }

  "XmlNode lenses" should {
    // TODO: ScalaCheck the plens laws
    "get element content" in {
      val content = XmlNode.elementContentPLens.get(
        XmlNodeElement("test", XmlAttribute("a", "b") :: Nil, List(
          XmlNodeElement("hello", Nil, Nil)
        ))
      )

      content must be some(List(
        XmlNodeElement("hello", Nil, Nil)
      ))
    }
    "set element content" in {
      val content = XmlNode.elementContentPLens.set(
        XmlNodeElement("test", XmlAttribute("a", "b") :: Nil, List(
          XmlNodeElement("hello", Nil, Nil)
        )),
        List.empty
      )

      content must be some(
        XmlNodeElement("test", XmlAttribute("a", "b") :: Nil, Nil)
      )
    }
    "get text content" in {
      val content = XmlNode.textContentPLens.get(
        XmlNodeText("world")
      )

      content must be some(
        "world"
      )
    }
    "set text content" in {
      val content = XmlNode.textContentPLens.set(
        XmlNodeText("world"),
        "hello"
      )

      content must be some(
        XmlNodeText("hello")
      )
    }
  }
}
