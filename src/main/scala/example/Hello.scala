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
object Hello extends App {

  import fastparse._
  val browser = JsoupBrowser()
  val doc2 = browser.get("https://github.com/sorrycc/awesome-javascript")
  def url =
    "https://raw.githubusercontent.com/sorrycc/awesome-javascript/master/README.md"
  val r = requests.get(url)
  val content: String = r.text

  val parser = MarkdownParser()

  val result = parser.parse(content)
  //val result = parser.toMarkdown("### ndudenude\n")
  result match {
    case Parsed.Success(markdown: Markdown, _) =>
      //println(markdown)
      //println(find(markdown.nodes, Nil))
      //findUnordered(markdown.nodes, Nil).foreach(println)
      //findUnordered(markdown.nodes, Nil).foreach(println)
      val x = findAllTyped(markdown.nodes, Seq.empty[Header])
      println(x)
    case f @ Parsed.Failure(_, _, _) => f.trace()

  }
  @tailrec
  def find(nodes: Seq[BlockElement], acc: Seq[H2]): Seq[H2] = {
    nodes match {
      case Nil                    => acc.reverse
      case Seq(h2: H2, tail @ _*) => find(tail, h2 +: acc)
      case _                      => find(nodes.tail, acc)
    }
  }

  @tailrec
  def findAllTyped[T: ClassTag](
      nodes: Seq[BlockElement],
      acc: Seq[T]
  ): Seq[T] = {
    nodes match {
      case Nil => acc.reverse
      case Seq(head: T, tail @ _*) =>
        findAllTyped[T](tail, head +: acc)
      case _ =>
        findAllTyped[T](nodes.tail, acc)
    }
  }

  @tailrec
  def findUnordered(
      nodes: Seq[BlockElement],
      acc: Seq[UnorderedList]
  ): Seq[UnorderedList] = {
    nodes match {
      case Nil                               => acc.reverse
      case Seq(h2: UnorderedList, tail @ _*) => findUnordered(tail, h2 +: acc)
      case _                                 => findUnordered(nodes.tail, acc)
    }
  }
}

case class Project(
    name: String,
    website: String,
    github_readme: Option[String],
    topics: List[String]
)

case class Markdown(nodes: Seq[BlockElement])
trait BlockElement
trait Header extends BlockElement
case class H1(content: Span) extends Header
case class H2(content: Span) extends Header
case class H3(content: Span) extends Header
case class H4(content: Span) extends Header
case class H5(content: Span) extends Header
case class H6(content: Span) extends Header

trait MDList extends BlockElement
case class OrderedList(items: Seq[Span]) extends MDList
case class UnorderedList(items: Seq[Span]) extends MDList

case class Quote(element: Seq[SpanElement]) extends BlockElement
trait SpanElement extends Product with Serializable
case class Span(element: Seq[SpanElement]) extends BlockElement {
  def listLink = "a"
  def description = "b"
}
case class Text(text: String) extends SpanElement
case class Link(description: String, url: String) extends SpanElement
case class CrossRefence(description: String, url: String) extends SpanElement
case class Image(description: String, url: String) extends SpanElement

case class Table(headers: List[String], elements: List[String])
    extends BlockElement

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
  //def quote[_: P] = P((">" ~ multiSpace ~ span).rep)
  def unorderedListItem[_: P]: P[Span] = P(("*" | "-") ~ span)
  def unorderedList[_: P]: P[UnorderedList] = P(
    (unorderedListItem ~ lineEnding).rep(1).map(UnorderedList(_))
  )

  def span[_: P]: P[Span] = P(
    (img | crossReference | link | plainText).rep.map(Span(_))
  )
  def h1[_: P]: P[Header] = P("#" ~ span.map(H1(_)))
  def h2[_: P]: P[Header] = P("#".rep(2) ~ span.map(H2(_)))
  def h3[_: P]: P[Header] = P("#".rep(3) ~ span.map(H3(_)))
  def h4[_: P]: P[Header] = P("#".rep(4) ~ span.map(H4(_)))
  def h5[_: P]: P[Header] = P("#".rep(5) ~ span.map(H5(_)))
  def h6[_: P]: P[Header] = P("#".rep(6) ~ span.map(H6(_)))
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
