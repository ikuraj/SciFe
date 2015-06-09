package scife.util.structures.riff

object RiffFormat {

//  case class RiffFormat(
//    authorName: String, title: String,
//    cFrames: Int, cSteps: Int, jiffRate: Int,
//    icons: List[Icon])

  trait Chunk {
    override def toString =
      datasizes(this).mkString("[", ",", "]") + "/" +
      jiffs(this).mkString("[", ",", "]") + "/" +
      audios(this).mkString("[", ",", "]") + "/"
  }
  case object Leaf extends Chunk
  case class Payload(data: Int, jiff: Int, isAudio: Int) extends Chunk
  case class Node(data: Int, left: Chunk, right: Chunk) extends Chunk

  def invariant(tree: Chunk, _size: Int, _totalPayload: Int, _totalLoss: Int, AVRatio: Int) =
    _totalPayload == totalPayload(tree) &&
    _totalLoss == totalLoss(tree) &&
    countAudio(tree) <= AVRatio * 64 &&
    ordering(tree) && sumCorrect(tree) && size(tree) == _size
    
  def totalPayload(tree: Chunk): Int = tree match {
    case Leaf => 0
    case Payload(d, _, _) => d
    case Node(_, l, r) =>
      totalPayload(l) + totalPayload(r)
  }
  
  def totalLoss(tree: Chunk): Int = tree match {
    case Leaf => 0
    case Payload(d, jiff, _) => d / 4 * jiff
    case Node(_, l, r) =>
      totalLoss(l) + totalLoss(r)
  }
  
  def countAudio(tree: Chunk): Int = tree match {
    case Payload(d, _, 1) => d
    case Node(_, l, r) =>
      countAudio(l) + countAudio(r)
    case _ => 0
  }

  def ordering(tree: Chunk): Boolean = tree match {
    case Node(_, l@Node(nl, _, _), r@Node(nr, _ ,_)) =>
      nr >= nl && ordering(l) && ordering(r)
    case _ => true
  }

  def sumCorrect(tree: Chunk): Boolean = tree match {
    case Node(n, l@Node(nl, _, _), r@Node(nr, _ ,_)) =>
      n == nr + nl && sumCorrect(l) && sumCorrect(r)
    case Node(n, l@Node(nl, _, _), _:Payload) =>
      n == nl && sumCorrect(l)
    case Node(n, _: Payload, r@Node(nr, _ ,_)) =>
      n == nr && sumCorrect(r)
    case _ => true
  }

  def size(t: Chunk): Int = t match {
    case _: Payload => 1
    case Node(_, l, r) => 1 + size(l) + size(r)
    case Leaf => 0
  }

  def datasizes(t: Chunk): List[Int] = t match {
    case Payload(d, _, _) => d :: Nil
    case Node(d, l, r) =>
      d :: datasizes(l) ::: datasizes(r)
    case _ => Nil
  }
  
  def jiffs(t: Chunk): List[Int] = t match {
    case Payload(_, j, _) => j :: Nil
    case Node(_, l, r) =>
      jiffs(l) ::: jiffs(r)
    case _ => Nil
  }
  
  def audios(t: Chunk): List[Int] = t match {
    case Payload(_, _, a) => a :: Nil
    case Node(_, l, r) =>
      audios(l) ::: audios(r)
    case _ => Nil
  }

}