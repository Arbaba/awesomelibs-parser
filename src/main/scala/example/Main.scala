package example
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
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

  val result = parser.parse(content, Some("## Package Managers"))

  /*val result = fastparse.parse(
    "# ndudenude\n",
    parser.h1(_),
    verboseFailures = true
  )
  println(result)*/
  result match {
    case Parsed.Success(markdown: Markdown, _) =>
      println(markdown)
      val x = findAllTyped[Link](markdown.nodes, Seq.empty[Link])
      // x.foreach(y => println(y))
      println(x.size)
      val y = findAll(
        markdown.nodes,
        n =>
          n match {
            case _: Header | UnorderedList(_) => true
            case _                            => false
          },
        Seq.empty[Node]
      )
      y.reverse.foreach(println)

    case f @ Parsed.Failure(_, _, _) => f.trace()

  }

  /** Find all nodes of the given paremetric Type T in the given list of nodes.
    * The functions also iterates over children of each node
    * @param nodes
    * @param acc
    * @return
    */
  @tailrec
  def findAllTyped[T: ClassTag](
      nodes: Seq[Node],
      acc: Seq[T]
  ): Seq[T] = {
    nodes match {
      case Nil => acc
      case Seq(head: T, tail @ _*) =>
        head.children match {
          case Nil =>
            findAllTyped[T](tail, head +: acc)
          case Seq(childrenHead, childrenTail @ _*) =>
            findAllTyped[T](childrenTail ++: tail, head +: acc)
        }
      case Seq(head, tail @ _*) =>
        findAllTyped[T](head.children ++: tail, acc)

    }
  }

  /** Find all nodes of for which the given predicate evaluates to true. The
    * functions also iterates over children of each node
    * @param nodes
    * @param predicate
    * @param acc
    * @return
    */
  @tailrec
  def findAll(
      nodes: Seq[Node],
      predicate: Node => Boolean,
      acc: Seq[Node]
  ): Seq[Node] = {
    nodes match {
      case Nil => acc
      case Seq(head, tail @ _*) if predicate(head) =>
        head.children match {
          case Nil =>
            findAll(tail, predicate, head +: acc)
          case Seq(childrenHead, childrenTail @ _*) =>
            findAll(childrenTail ++: tail, predicate, head +: acc)
        }
      case Seq(head, tail @ _*) =>
        findAll(head.children ++: tail, predicate, acc)

    }
  }
}

case class Project(
    name: String,
    website: String,
    github_readme: Option[String],
    topics: List[String]
)
