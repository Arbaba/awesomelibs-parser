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
object Main extends App {

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

