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

case class Markdown(nodes: Seq[BlockElement])

trait Node {
  def children: Seq[Node] = Nil
}
trait BlockElement extends Node

trait Header extends BlockElement
case class H1(content: Span) extends Header
case class H2(content: Span) extends Header
case class H3(content: Span) extends Header
case class H4(content: Span) extends Header
case class H5(content: Span) extends Header
case class H6(content: Span) extends Header

trait MDList extends BlockElement
case class OrderedList(override val children: Seq[Span]) extends MDList
case class UnorderedList(override val children: Seq[Span]) extends MDList

case class Quote(override val children: Seq[SpanElement]) extends BlockElement
case class Span(override val children: Seq[SpanElement]) extends BlockElement

trait SpanElement extends Node with Product with Serializable
case class Text(text: String) extends SpanElement
case class Italic(text: String) extends SpanElement

case class Link(description: String, url: String) extends SpanElement
case class CrossRefence(description: String, url: String) extends SpanElement
case class Image(description: String, url: String) extends SpanElement
