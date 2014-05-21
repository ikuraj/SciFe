package insynth.util

import RBTreeItems._


/*** Okasaki-style red-black tree maps. ***/

// Based on:
// http://www.eecs.usma.edu/webs/people/okasaki/jfp99.ps

/*
 
 Red-black trees are binary search trees obeying two key invariants:

 (1) Any path from a root node to a leaf node contains the same number
     of black nodes. (A balancing condition.)

 (2) Red nodes always have black children.

 */


private object RBTreeItems {
  class Color
  case object R extends Color // Red.
  case object B extends Color // Black.
}


/* Internally, a map is a tree node; there is no wrapper. */

abstract class RBMap[K, V] (implicit cmp : K => Ordered[K]) {

  /* We could have required that K be <: Ordered[K], but this is
  actually less general than requiring an implicit parameter that can
  convert K into an Ordered[K].

  For example, Int is not compatible with Ordered[Int].  This would
  make it unusable with this map; however, it's simple to define an
  injector from Int into Ordered[Int].

  In fact, the standard prelude already defines just such an implicit:
  intWrapper. */

  // blacken: Turn a node black.
  protected def blacken (n : RBMap[K,V])  : RBMap[K,V] = {
    n match {
      case L() => n
      case T(_,l,k,v,r) => T(B,l,k,v,r)
    }
  }
  
  // balance: Balance a tree with balanced subtrees.
  protected def balance (c : Color) (l : RBMap[K,V]) (k : K) (v : Option[V]) (r : RBMap[K,V]) : RBMap[K,V] = {
    (c,l,k,v,r) match {
      case (B,T(R,T(R,a,xK,xV,b),yK,yV,c),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,T(R,a,xK,xV,T(R,b,yK,yV,c)),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,T(R,b,yK,yV,c),zK,zV,d)) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,b,yK,yV,T(R,c,zK,zV,d))) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (c,a,xK,xV,b) => T(c,a,xK,xV,b)
    }
  }

  // modWith: Helper method; top node could be red.
  private[util] def modWith (k : K, f : (K, Option[V]) => Option[V]) : RBMap[K,V] 

  // modifiedWith: Insert, update and delete all in one. 
  def modifiedWith (k : K, f : (K, Option[V]) => Option[V]) : RBMap[K,V] = 
    blacken(modWith(k,f))

  // get: Retrieve a value for a key.
  def get(k : K) : Option[V]

  // insert: Insert a value at a key.
  def insert (k : K, v : V) = modifiedWith (k, (_,_) => Some(v))

  // remove: Delete a key.
  def remove (k : K) = modifiedWith (k, (_,_) => None)
}


// A leaf node.
private case class L[K, V] (implicit cmp : K => Ordered[K]) extends RBMap[K,V]  {
  def get(k : K) : Option[V] = None

  private[util] def modWith (k : K, f : (K, Option[V]) => Option[V]) : RBMap[K,V] = {
    T(R, this, k, f(k,None), this)
  }
}


// A tree node.
private case class T[K, V](c : Color, l : RBMap[K,V], k : K, v : Option[V], r : RBMap[K,V]) (implicit cmp : K => Ordered[K]) extends RBMap[K,V] {
  def get(k : K) : Option[V] = {
    if (k < this.k) l.get(k) else
    if (k > this.k) r.get(k) else
    v 
  }

  private[util] def modWith (k : K, f : (K, Option[V]) => Option[V]) : RBMap[K,V] = {
    if (k <  this.k) (balance (c) (l.modWith(k,f)) (this.k) (this.v) (r)) else
    if (k == this.k) (T(c,l,k,f(this.k,this.v),r)) else
    (balance (c) (l) (this.k) (this.v) (r.modWith(k,f)))
  }
}


// A helper object.
object RBMap {
  
  // empty: Converts an orderable type into an empty RBMap.
  def empty[K <: Ordered[K], V] : RBMap[K,V] = L()((k : K) => k)

  // apply: Assumes an implicit conversion.
  def apply[K, V](args : (K,V)*)(implicit cmp : K => Ordered[K]) : RBMap[K,V] = {
    var currentMap : RBMap[K,V] = L()
    for ((k,v) <- args) {
      currentMap = currentMap.insert(k,v)
    }
    currentMap
  }
}
