// taken from https://github.com/melvic-ybanez/scala-bst
package scife.util.structures.bst

import scala.collection.immutable.Queue

package ybanez {

  trait BST[+A] {
    def +[B >: A <% Ordered[B]](elem: B): BST[B]
    def ++[B >: A <% Ordered[B]](bst: BST[B]): BST[B]
    def -[B >: A <% Ordered[B]](elem: B): (Option[B], BST[B])

    def exists(p: A => Boolean): Boolean
    def contains[B >: A <% Ordered[B]](e: B): Boolean
    def filter[B >: A <% Ordered[B]](p: A => Boolean): BST[B] = filterAcc[B](EmptyBST)(p)
    def filterAcc[B >: A <% Ordered[B]](acc: BST[B])(p: A => Boolean): BST[B]

    def flatMap[B <% Ordered[B]](f: A => BST[B]): BST[B]
    def map[B <% Ordered[B]](f: A => B): BST[B]

    def inOrder[B](z: B)(f: (A, B) => B): B
    def preOrder[B](z: B)(f: (A, B) => B): B
    def postOrder[B](z: B)(f: (A, B) => B): B
    def levelOrder[B](z: B)(f: (A, B) => B): B

    def withLeft[B >: A <% Ordered[B]](newLeft: BST[B]): BST[B]
    def withRight[B >: A <% Ordered[B]](newRight: BST[B]): BST[B]
    def orElse[B >: A <% Ordered[B]](tree: BST[B]): BST[B]
    def minChild[B >: A <% Ordered[B]]: BST[B] = minChildAcc[B](this)
    def minChildAcc[B >: A <% Ordered[B]](acc: BST[B]): BST[B]

    def toList = preOrder(List[A]())(_ :: _).reverse
    
    def size = preOrder(0)( { case (x, y) => y + 1  } )
    
  }

  case object EmptyBST extends BST[Nothing] {
    def +[B <% Ordered[B]](elem: B) = BST(elem)
    def ++[B <% Ordered[B]](bst: BST[B]) = bst
    def -[B <% Ordered[B]](elem: B) = (None, EmptyBST)

    def flatMap[B <% Ordered[B]](f: Nothing => BST[B]): BST[B] = EmptyBST
    def map[B <% Ordered[B]](f: Nothing => B): BST[B] = EmptyBST

    def exists(p: Nothing => Boolean) = false
    def contains[B <% Ordered[B]](e: B) = false
    def filterAcc[B <% Ordered[B]](acc: BST[B])(p: Nothing => Boolean) = acc

    def inOrder[B](z: B)(f: (Nothing, B) => B) = z
    def preOrder[B](z: B)(f: (Nothing, B) => B) = z
    def postOrder[B](z: B)(f: (Nothing, B) => B) = z
    def levelOrder[B](z: B)(f: (Nothing, B) => B) = z

    def withLeft[B <% Ordered[B]](newLeft: BST[B]) = newLeft
    def withRight[B <% Ordered[B]](newRight: BST[B]) = newRight
    def orElse[B <% Ordered[B]](tree: BST[B]) = tree
    def minChildAcc[B <% Ordered[B]](acc: BST[B]) = acc

    override def toString = "Leaf"
  }

  case class NonEmptyBST[A <% Ordered[A]](elem: A, left: BST[A], right: BST[A]) extends BST[A] {
    def this(left: BST[A], elem: A, right: BST[A]) = this(elem, left, right)
    
    def +[B >: A <% Ordered[B]](newElem: B) =
      if (newElem < elem) withLeft(left + newElem)
      else if (newElem > elem) withRight(right + newElem)
      else this

    def ++[B >: A <% Ordered[B]](bst: BST[B]) = bst.preOrder[BST[B]](this)((e, acc) => acc + e)

    def -[B >: A <% Ordered[B]](e: B) =
      if (e < elem) (left - e) match {
        case (opt, l) => (opt, withLeft(l))
      }
      else if (e > elem) (right - e) match {
        case (opt, r) => (opt, withRight(r))
      }
      else (Some(elem), (left, right) match {
        case (EmptyBST, EmptyBST) => EmptyBST
        case (l, EmptyBST) => l
        case (EmptyBST, r) => r
        case (l, r) => right.minChild match {
          case EmptyBST => r.withLeft(l)
          case NonEmptyBST(min, _, _) => NonEmptyBST(min, l, (r - min)._2)
        }
      })

    def exists(p: A => Boolean) = p(elem) || left.exists(p) || right.exists(p)
    def contains[B >: A <% Ordered[B]](e: B) = exists(_ == e)
    def filterAcc[B >: A <% Ordered[B]](acc: BST[B])(p: A => Boolean) =
      right.filterAcc(left.filterAcc(if (p(elem)) acc + elem else acc)(p))(p)

    def flatMap[B <% Ordered[B]](f: A => BST[B]) = preOrder(f(elem))((e, acc) => acc ++ f(e))
    def map[B <% Ordered[B]](f: A => B) = preOrder[BST[B]](BST(f(elem)))((e, acc) => acc + f(e))

    def inOrder[B](z: B)(f: (A, B) => B) = right.inOrder(f(elem, left.inOrder(z)(f)))(f)
    def preOrder[B](z: B)(f: (A, B) => B) = right.preOrder(left.preOrder(f(elem, z))(f))(f)
    def postOrder[B](z: B)(f: (A, B) => B) = f(elem, right.postOrder(left.postOrder(z)(f))(f))

    def levelOrder[B](z: B)(f: (A, B) => B) = {
      def recurse(acc: B, queue: Queue[BST[A]]): B = queue match {
        case Queue() => acc
        case h +: t => h match {
          case EmptyBST => recurse(acc, t)
          case NonEmptyBST(e, l, r) => recurse(f(e, acc), t.enqueue(l).enqueue(r))
        }
      }

      recurse(z, Queue(this))
    }

    def withLeft[B >: A <% Ordered[B]](newLeft: BST[B]) = NonEmptyBST(elem, newLeft, right)
    def withRight[B >: A <% Ordered[B]](newRight: BST[B]) = NonEmptyBST(elem, left, newRight)
    def minChildAcc[B >: A <% Ordered[B]](acc: BST[B]) = left.minChildAcc(this)
    def orElse[B >: A <% Ordered[B]](tree: BST[B]) = this

    override def toString = elem + "[l=" + left + ", r=" + right + "]"
    
//    private[this] var _hashCode: Int = 0
//    override def hashCode(): Int =
//      if (_hashCode != 0) _hashCode
//      else {
//        _hashCode = scala.runtime.ScalaRunTime._hashCode(this)
//        _hashCode
//      }
//    override def equals(x: Any): Boolean =
//      this eq x.asInstanceOf[AnyRef]
//      System.identityHashCode(x) == System.identityHashCode(this)
    
    override val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  object BST {
    def apply[A <% Ordered[A]](): BST[A] = EmptyBST

    def apply[A <% Ordered[A]](elem: A, elems: A*): BST[A] = {
      def recurse(elems: List[A], bst: BST[A]): BST[A] =
        if (elems.isEmpty) bst
        else recurse(elems.tail, bst + elems.head)

      recurse(elems.toList, NonEmptyBST(elem, EmptyBST, EmptyBST))
    }
    
    def Node[A <% Ordered[A]](left: BST[A], elem: A, right: BST[A]) = NonEmptyBST(elem, left, right)

    def Node[A <% Ordered[A]](elem: A) = BST(elem)
    
    import scife.util.structures._
    
    def thisBST2simpleBST(tree: BST[Int]): BSTrees.Tree = tree match {
      case EmptyBST => BSTrees.Leaf
      case NonEmptyBST(v, l, r) =>
        BSTrees.Node(thisBST2simpleBST(l), v, thisBST2simpleBST(r))
    }
  }

}