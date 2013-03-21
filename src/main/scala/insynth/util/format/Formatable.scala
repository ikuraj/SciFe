package insynth.util.format

// enable implicit conversions
import scala.language.implicitConversions

import scala.text.Document
import Document._
import scala.text.DocNil

trait Formatable {
  def toDocument: Document

  def println() = {
    val stdWriter = new java.io.OutputStreamWriter(System.out)
    this.toDocument.format(140, stdWriter)
    stdWriter.write('\n')
    stdWriter.flush()
  }

  override def toString = {
    val stringWriter = new java.io.StringWriter
    this.toDocument.format(140, stringWriter)
    stringWriter.flush()
    stringWriter.toString
  }

}

object FormatHelpers {

  def paren(d: Document): Document =
    group("(" :: d :: ")")

  def brackets(d: Document): Document =
    group("{" :/: d :/: "}")

  def nestedParen(d: Document): Document =
    group("(" :: nest(1, break :: d) :/: ")")

  def nestedBrackets(d: Document): Document =
    group("{" :: nest(1, break :: d) :/: "}")

  def sqBrackets(d: Document) =
    group("[" :: d :: "]")

  def nestedSqBrackets(d: Document) =
    group("[" :: nest(1, break :: d) :: "]")

  def addOrEmpty(d1: Document, d2: Document): Document =
    d1 match {
      case `DocNil` => empty
      case _ => d1 :: d2
    }

  //  def taggedParen2(tag: String, d1: Document, d2: Document) =
  //    taggedParen(tag, d1 :: nest(-tag.length, break :: d2))

  def foldDoc(docs: List[Document], sep: Document): Document = docs match {
    case Nil => empty
    case d :: Nil => d
    case d :: ds => d :: sep :/: foldDoc(ds, sep)
    case d => throw new RuntimeException("asd" + d)
  }

  def seqToDoc[T](docs: List[T], sep: String, toDoc: T => Document): Document =
    foldDoc(docs map toDoc, sep)

  def seqToDoc[T](docs: List[T], sep: Document, toDoc: T => Document): Document =
    foldDoc(docs map toDoc, sep)

  def seqToDoc[T](docs: List[T], toDoc: T => Document): Document =
    foldDoc(docs map toDoc, "")

  implicit def strToDoc(s: String): Document = text(s)
  implicit def intToDoc(i: Int): Document = text(i.toString)
}
