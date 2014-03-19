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
  }
}
