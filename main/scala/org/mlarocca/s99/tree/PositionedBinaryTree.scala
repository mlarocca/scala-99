package org.mlarocca.s99.tree

trait PositionedBinaryTree[+K, +V] extends BinaryTree[K, V] {
  private[tree] def toInOrderItemList[T >: K, W >: V](): Seq[PositionedItem[T, W]]
  def itemsAtLevel[T >: K, W >: V](level: Int): Seq[PositionedItem[T, W]]
  def leftMostX(): Int
  def rightMostX(): Int
  private[tree] def leftBoundaries(): Map[Int, PositionedBinaryTree[K, V]]
  private[tree] def rightBoundaries(): Map[Int, PositionedBinaryTree[K, V]]
  private[tree] def compact(): PositionedBinaryTree[K, V]
}

case object PositionedBinaryLeaf extends Leaf with PositionedBinaryTree[Nothing, Nothing] {
  override private[tree] def toInOrderItemList[T, W ]() = Nil
  override def itemsAtLevel[T, W](level: Int) = Nil
  override def leftMostX(): Int = 0
  override def rightMostX(): Int = 0
  override private[tree] def compact() = PositionedBinaryLeaf
  override private[tree] def leftBoundaries(): Map[Int, Int] = Map.empty.withDefaultValue(0)
  override private[tree] def rightBoundaries(): Map[Int, Int] = Map.empty.withDefaultValue(0)
}

class PositionedBinaryNode[+K, +V](override val key: K, override val left: PositionedBinaryTree[K, V], override val right: PositionedBinaryTree[K, V], override val value: Option[V], x: Int, y: Int) extends BinaryNode[K, V](key, left, right, value) with PositionedBinaryTree[K, V] {
  private lazy val bounds = (left, right) match {
    case (PositionedBinaryLeaf, PositionedBinaryLeaf) => 1 -> (x + 1, x - 1)
    case (leftTree: PositionedBinaryNode, PositionedBinaryLeaf) => {
      leftTree.leftBoundaries().map {
        case (level, (leftBound, rightBound)) =>
          (level + 1 ->(leftBound, rightBound))
      } +(1 -> (leftTree.x, Math.max(leftTree.x, x - 1)))
    }
    case (PositionedBinaryLeaf, rightTree: PositionedBinaryNode) => {
      rightTree.leftBoundaries().map {
        case (level, (leftBound, rightBound)) =>
          (level + 1 -> (leftBound, rightBound))
      } + (1 -> (Math.min(rightTree.x, x + 1), rightTree.x))
    }
    case (PositionedBinaryLeaf, rightTree: PositionedBinaryNode) => {
      rightTree.leftBoundaries().map {
        case (level, (leftBound, rightBound)) =>
          (level + 1 -> (leftBound, rightBound))
      } + (1 -> (Math.min(rightTree.x, x + 1), rightTree.x))
    }

  }

  private lazy val rightBounds = right.rightBoundaries().map {
    case (level, node) =>
      (level + 1 -> node)
  } + (1 -> right)

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

  override private[tree] def leftBoundaries(): Map[Int, PositionedBinaryTree[K, V]] = leftBounds

  override private[tree] def rightBoundaries(): Map[Int, PositionedBinaryTree[K, V]] = rightBounds

  override private[tree] def compact(): PositionedBinaryTree[K, V] = {

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