package networkaddonmod.localization

import org.scalatest.{WordSpec, Matchers}

class GenerateLocalesSpec extends WordSpec with Matchers {

  val poTestCase = raw"""
# comment
msgctxt "context1"
msgid "aaa"
msgstr "AAA"

#, fuzzy
#| msgctxt "context2"
#| msgid "bbb-old"
msgctxt "context2"
msgid "bbb"
msgstr "BBB-old"

msgctxt "context3"
msgid "ccc"
msgstr "CCC"
"""

  "workaround for detecting fuzzy translations" should {
    "work as expected" in {
      // This test ensures that scaposer's parsing of comments has not changed so our workaround keeps working
      val Right(translations) = scaposer.Parser.parse(poTestCase)
      // fuzzy flag is read as part of first entry, not second
      translations.map(_.asInstanceOf[scaposer.SingularTranslation].otherComments.exists(_.contains("fuzzy"))) shouldBe Seq(true, false, false)
      // in particular, ctxComments of second entry are empty
      translations.map(_.asInstanceOf[scaposer.SingularTranslation].ctxComments.nonEmpty) shouldBe Seq(true, false, false)
    }
  }
}
