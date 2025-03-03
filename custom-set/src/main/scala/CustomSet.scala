import scala.collection.mutable

final case class CustomSet(data: mutable.BitSet)

object CustomSet:
  def fromList(list: List[Int]): CustomSet =
    new CustomSet(mutable.BitSet(list *))

  def empty(customSet: CustomSet): Boolean =
    customSet.data.isEmpty

  def member(customSet: CustomSet, element: Int): Boolean =
    customSet.data.contains(element)

  def isSubsetOf(a: CustomSet, b: CustomSet): Boolean = ???

  def isDisjointFrom(a: CustomSet, b: CustomSet): Boolean = ???

  def union(a: CustomSet, b: CustomSet): CustomSet = ???

  def intersection(a: CustomSet, b: CustomSet): CustomSet = ???
  
  def difference(a: CustomSet, b: CustomSet): CustomSet = ???
  
  def isEqual(a: CustomSet, b: CustomSet): Boolean = ???
  
  def insert(customSet: CustomSet, element: Int): CustomSet = ???
  