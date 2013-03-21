package insynth.structures

import insynth.util.format.FormatSuccinctType

sealed abstract class SuccinctType {
  override def toString = FormatSuccinctType(this).toString
}

object BottomType extends SuccinctType

//--------------------------------------------------- Ground Types ----------------------------------------------------------//
  
case class Const(val name: String) extends SuccinctType
case class Instance(val name: String, val t: List[SuccinctType]) extends SuccinctType
case class Arrow(val paramType:TSet, val returnType:SuccinctType) extends SuccinctType

// TODO: Maybe find better representation
case class TSet(val list:List[SuccinctType]) extends SuccinctType {  
  
  def this() = this(Nil)
  
  private var hashCodeValue:Int = list.map(x => x.hashCode >>> 4).foldLeft(2482783)( _+_) ^ 23821
  
  override def equals(that:Any) = {
    if (that == null || !that.isInstanceOf[TSet]) false
    else {
      val tpe = that.asInstanceOf[TSet]
      TSet.equals(this, tpe)
    }
  }
  
  override def hashCode() = hashCodeValue
  
  def subsetOf(tpe:TSet) = {
    if (tpe == null) false
    else {
      TSet.subsetOf(this,tpe)
    }
  }
  
  def minus(tpe1: TSet) = TSet.minus(this, tpe1)
  
  def union(tpe1: TSet) = TSet.union(this, tpe1)
  
  def contains(tpe1: SuccinctType) = list.contains(tpe1)
  
  def content = list
  
}

object TSet {

  val empty = new TSet()
  
  def apply(tpe: SuccinctType*) = new TSet(tpe.toList)
  
  def equals(tpe1:TSet, tpe2:TSet) = {
    val length1 = tpe1.list.length
    val length2 = tpe2.list.length
    
    if (length1 != length2) false
    else {
      tpe1.list.forall(x => tpe2.list.contains(x))
    }
  }
  
  def subsetOf(tpe1:TSet, tpe2:TSet) = {
    val length1 = tpe1.list.length
    val length2 = tpe2.list.length
    
    if (length1 > length2) false
    else {
      tpe1.list.forall(x => tpe2.list.contains(x))
    }
  }
  
  def union(tpe1:TSet, tpe2:TSet) = TSet(tpe1.list ::: tpe2.list.filterNot(tpe1.list.contains))
  
  def minus(tpe1:TSet, tpe2:TSet) = TSet(tpe1.list.filterNot(tpe2.list.contains))
  
}

object SuccinctType {
  
  def returnType(tpe: SuccinctType) = tpe match {
    case Arrow(_,returnType) => returnType
    case IArrow(_,returnType) => returnType
    case t => t
  }
  
  def paramTypes(tpe: SuccinctType) = tpe match {
    case Arrow(params,_) => params.content
    case IArrow(params,_) => params.content
    case _ => Nil
  }
  
  def paramSetType(tpe: SuccinctType) = tpe match {
    case Arrow(params,_) => params
    case IArrow(params,_) => params
    case _ => TSet.empty
  }
}

//------------------------------------------------ Polymorphic Types --------------------------------------------------------//
  
case class Variable(val name:String) extends SuccinctType

//-------------------------------------------- Inheritance Function Types ---------------------------------------------------//

case class IArrow(val subclass:TSet, val superclass: SuccinctType) extends SuccinctType