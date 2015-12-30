package org.mlarocca.s99.tree

trait PositionedBinaryTree[+K, +V] extends BinaryTree[K, V] {
  private[tree] def toInOrderItemList[T >: K, W >: V](): Seq[PositionedItem[T, W]] = Nil
  def itemsAtLevel[T >: K, W >: V](level: Int): Seq[PositionedItem[T, W]] = Nil
}

case object PositionedBinaryLeaf extends Leaf with PositionedBinaryTree[Nothing, Nothing]

class PositionedBinaryNode[+K, +V](override val key: K, override val left: PositionedBinaryTree[K, V], override val right: PositionedBinaryTree[K, V], override val value: Option[V], x: Int, y: Int) extends BinaryNode[K, V](key, left, right, value) with PositionedBinaryTree[K, V] {

  //Combination of preorder + inorder is unique for each tree (trees with the same keys set could have the same inorder or preorder)
  override def hashCode = this.toString.hashCode()

  override def canEqual(other: Any): Boolean = {
    other.isInstanceOf[PositionedBinaryNode[K, V]]
  }

  override def equals(other: Any) = other match {
    case that: BinaryNode[K, V] => (that canEqual this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def toString = s"T[$x, $y]($key $left $right)"

  override private[tree] def toInOrderItemList[T >: K, W >: V](): Seq[PositionedItem[T, W]] = {
    left.toInOrderItemList[T, W]() ++ (PositionedItem[T, W](key, value, x, y) +: right.toInOrderItemList[T, W]())
  }

  @throws[IllegalArgumentException]
  override def itemsAtLevel[T >: K, W >: V](level: Int): Seq[PositionedItem[T, W]] = level match {
    case 1 =>
      Seq(PositionedItem[T, W](key, value, x, y))
    case _ if level > 1 =>
      left.itemsAtLevel[T, W](level - 1) ++ right.itemsAtLevel[T, W](level - 1)
    case _ =>
      throw new IllegalArgumentException(BinaryTree.NonPositiveValueErrorMessage)
  }
}

object PositionedBinaryNode {
  def apply[K, V](key: K, x: Int, y: Int) =
    new PositionedBinaryNode[K, V](key, PositionedBinaryLeaf, PositionedBinaryLeaf, None, x, y)
  def apply[K, V](key: K, value: V, x: Int, y: Int) =
    new PositionedBinaryNode[K, V](key, PositionedBinaryLeaf, PositionedBinaryLeaf, Some(value), x, y)
}

/**
 * An item stored in a PositionedTree's node.
 * Any item is described by four parameters:
 *
 * @param key The key.
 * @param value The value.
 * @param x The x position of the node in the tree layout.
 * @param y The y position of the node in the tree layout. (I.e. its height)
 * @tparam K The type of the keys.
 * @tparam V The type of the values.
 */
case class PositionedItem[K, V](key: K, value: Option[V], x: Int, y:Int)
