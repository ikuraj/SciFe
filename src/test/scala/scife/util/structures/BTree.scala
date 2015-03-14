package scife.util.structures

object BTree {

  case class Tree(keys: List[Int], children: List[Tree]) {
    require { keys.size + 1 == children.size }
  }

  def invariant(tree: Tree, min: Int, max: Int, t: Int) =
    valueOrdering(tree) && valuesInRange(tree, min, max) &&
      correctT(tree, t)
  

  def valuesInRange(t: Tree, min: Int, max: Int): Boolean = t match {
    case Tree(keys, children) =>
      keys.forall(v => min <= v && max >= v) && children.forall(c => valuesInRange(c, min, max))
  }

  def valueOrdering(t: Tree): Boolean = {
    val Tree(keys, children) = t

    keys.zip(keys.tail).forall( { case (a, b) => a < b } ) &&
    keys.zip(keys.tail).zip(children).forall(( { case ((a, b), c) => valuesInRange(c, a, b) }))

  }
  
  def correctT(tree: Tree, t: Int) = {
    def rec(n: Tree): Boolean = {
      import n._
      
      keys.size <= 2 * t - 1 &&
      keys.size >= t - 1 &&
      children.forall(rec)
    }
    
    size(tree) <= 2 * t - 1 && size(tree) == tree.keys.size || rec(tree)
  }

  def size(t: Tree): Int = t match {
    case Tree(keys, children) =>
      keys.size + children.map(size(_)).sum
  }

}