package example

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import fastparse.Parsed.Failure
import scala.annotation.tailrec
import scala.reflect.ClassTag
import fastparse.Parsed

trait Parser[I, O] {
  def parse(input: I, startFrom: Option[I] = None, endAt: Option[I]): Parsed[O]
}

case class MarkdownParser() extends Parser[String, Markdown] {
  import fastparse._, SingleLineWhitespace._

  def multiSpace[_: P] = P(" ".rep(1))
  def lineEnding[_: P] = P("\r\n" | "\n")
  def unclosedDescr[_: P]: P[Text] =
    (CharsWhile(c => c != '\n' && c != '\r' && c != ']')).!.map(Text(_))
  def unclosedURL[_: P]: P[Text] =
    (CharsWhile(c => c != '\n' && c != '\r' && c != ')')).!.map(Text(_))
  def img[_: P]: P[Image] = P(
    ("![" ~/ unclosedDescr ~ "](" ~ unclosedURL ~ ")").map {
      case (Text(description), Text(url)) => Image(description, url)
    }
  )
  def link[_: P]: P[Link] = P(
    ("[" ~ unclosedDescr ~ "](" ~ unclosedURL ~ ")").map {
      case (Text(description), Text(url)) =>
        Link(description, url)
    }
  )
  def header[_: P]: P[Header] = P(h6 | h5 | h4 | h3 | h2 | h1)
  def plainText[_: P]: P[Text] = P(
    (CharsWhile(c => c != '\n' && c != '\r')).!.map(Text(_))
  )
  def crossReference[_: P]: P[CrossRefence] = P(
    ("[" ~ unclosedDescr ~ "](#" ~ unclosedURL ~ ")").map {
      case (Text(description), Text(url)) =>
        CrossRefence(description, url)
    }
  )
  def unclosedItalic[_: P]: P[Italic] =
    (CharsWhile(c => c != '\n' && c != '\r' && c != '*')).!.map(Italic(_))
  def italic[_: P]: P[Italic] = P(
    ("*" ~~ unclosedItalic ~~ "*")
  )
  //def quote[_: P] = P((">" ~ multiSpace ~ span).rep)
  def unorderedListItem[_: P]: P[Span] = P(("*" | "-") ~~ multiSpace ~ span)
  def unorderedList[_: P]: P[UnorderedList] = P(
    (unorderedListItem ~ lineEnding).rep(1).map(UnorderedList(_))
  )

  def span[_: P]: P[Span] = P(
    (img | crossReference | link | italic | plainText).rep.map(Span(_))
  )
  def h1[_: P]: P[Header] = P("#" ~~ multiSpace ~ span.map(H1(_)))
  def h2[_: P]: P[Header] = P("#".rep(2) ~~ multiSpace ~ span.map(H2(_)))
  def h3[_: P]: P[Header] = P("#".rep(3) ~~ multiSpace ~ span.map(H3(_)))
  def h4[_: P]: P[Header] = P("#".rep(4) ~~ multiSpace ~ span.map(H4(_)))
  def h5[_: P]: P[Header] = P("#".rep(5) ~~ multiSpace ~ span.map(H5(_)))
  def h6[_: P]: P[Header] = P("#".rep(6) ~~ multiSpace ~ span.map(H6(_)))
  def mmm[_: P]: P[Span] = span
  def parseBlock[_: P]: P[BlockElement] = P(
    (unorderedList | ((header | span) ~ lineEnding))
  )
  def parseMarkDown[_: P]: P[Markdown] = P(
    ((parseBlock).rep(1) ~ End).map(Markdown(_))
  )

  def parse(
      rawMarkdown: String,
      startFrom: Option[String] = None,
      endAt: Option[String] = None
  ): Parsed[Markdown] = {
    val text: String = startFrom match {
      case Some(needle) =>
        val index = rawMarkdown.indexOf(needle)
        rawMarkdown.substring(index)
      case None => rawMarkdown
    }
    fastparse.parse(
      text,
      parseMarkDown(_),
      verboseFailures = true
    )
  }
}
