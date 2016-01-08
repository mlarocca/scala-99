package org.mlarocca.s99.tree

trait PositionedBinaryTree[+K, +V] extends BinaryTree[K, V] {
  private[tree] def toInOrderItemList[T >: K, W >: V](): Seq[PositionedItem[T, W]]
  def itemsAtLevel[T >: K, W >: V](level: Int): Seq[PositionedItem[T, W]]
  def leftMostX(): Int
  def rightMostX(): Int
  private[tree] val bounds: Map[Int, Bound]
  private[tree] def moveTo(deltaX: Int, deltaY: Int): PositionedBinaryTree[K, V]
}

case object PositionedBinaryLeaf extends Leaf with PositionedBinaryTree[Nothing, Nothing] {
  override private[tree] def toInOrderItemList[T, W ]() = Nil
  override def itemsAtLevel[T, W](level: Int) = Nil
  override def leftMostX(): Int = 0
  override def rightMostX(): Int = 0
  override private[tree] val bounds: Map[Int, Bound] = Map.empty
  override private[tree] def moveTo(deltaX: Int, deltaY: Int) = PositionedBinaryLeaf
}

class PositionedBinaryNode[+K, +V](override val key: K, override val left: PositionedBinaryTree[K, V], override val right: PositionedBinaryTree[K, V], override val value: Option[V], val x: Int, val y: Int) extends BinaryNode[K, V](key, left, right, value) with PositionedBinaryTree[K, V] {
  import BinaryTree.{maxOption, minOption}

  override private[tree] lazy val bounds: Map[Int, Bound] = {
    val leftBounds = left.bounds
    val rightBounds = right.bounds
    val nextLevel: (Int, Bound) = y -> Bound(x, x)

    val maxH = Math.max(maxOption(leftBounds.keys, 0), maxOption(rightBounds.keys, 0))
    val childrenBoundaries = ((y + 1) to maxH).map { h =>
      val lBoundOption = leftBounds.get(h)
      val rBoundOption = rightBounds.get(h)

      h -> {
        (lBoundOption, rBoundOption) match {
          case (Some(lB), None) => lB
          case (None, Some(rB)) => rB
          case _ =>
            //INVARIANT: (None, None) is impossible by construction
            Bound(lBoundOption.get.left, rBoundOption.get.right)
        }
      }
    }.toMap

    childrenBoundaries + nextLevel
  }

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

  /**
   * Return the x position of the leftmost node in the subtree
   * @return
   */
  override def leftMostX(): Int = left match {
    case PositionedBinaryLeaf => x
    case _ => left.leftMostX()
  }

  /**
   * Return the x position of the rightmost node in the subtree
   * @return
   */
  override def rightMostX(): Int = right match {
    case PositionedBinaryLeaf => x
    case _ => right.rightMostX()
  }

  override private[tree] def moveTo(deltaX: Int, deltaY: Int): PositionedBinaryTree[K, V] = {
    new PositionedBinaryNode[K, V](key, left.moveTo(deltaX, deltaY), right.moveTo(deltaX, deltaY), value, x + deltaX, y + deltaY)
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

private[tree] case class Bound(left: Int, right: Int)