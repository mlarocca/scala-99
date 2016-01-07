package org.mlarocca.s99.tree

trait PositionedBinaryTree[+K, +V] extends BinaryTree[K, V] {
  private[tree] def toInOrderItemList[T >: K, W >: V](): Seq[PositionedItem[T, W]]
  def itemsAtLevel[T >: K, W >: V](level: Int): Seq[PositionedItem[T, W]]
  def leftMostX(): Int
  def rightMostX(): Int
  def compactTree(isRoot: Boolean = false): PositionedBinaryTree[K, V]
  private[tree] def moveToLeft(delta: Int, parentX: Int): PositionedBinaryTree[K, V]
  private[tree] val bounds: Map[Int, Bound]
}

case object PositionedBinaryLeaf extends Leaf with PositionedBinaryTree[Nothing, Nothing] {
  override private[tree] def toInOrderItemList[T, W ]() = Nil
  override def itemsAtLevel[T, W](level: Int) = Nil
  override def leftMostX(): Int = 0
  override def rightMostX(): Int = 0
  override def compactTree(isRoot: Boolean = false): PositionedBinaryTree[Nothing, Nothing] = PositionedBinaryLeaf
  override private[tree] def moveToLeft(delta: Int, parentX: Int) = PositionedBinaryLeaf
  override private[tree] val bounds: Map[Int, Bound] = Map.empty
}

class PositionedBinaryNode[+K, +V](override val key: K, override val left: PositionedBinaryTree[K, V], override val right: PositionedBinaryTree[K, V], override val value: Option[V], val x: Int, val y: Int) extends BinaryNode[K, V](key, left, right, value) with PositionedBinaryTree[K, V] {
  import PositionedBinaryNode.{maxOption, minOption}

  override private[tree] lazy val bounds: Map[Int, Bound] = {
    val leftBounds = left.bounds
    val rightBounds = right.bounds
    val nextLevel: (Int, Bound) = 1 -> {
      (left, right) match {
        case (PositionedBinaryLeaf, PositionedBinaryLeaf) => Bound(x, x)
        case (leftTree: PositionedBinaryNode[K, V], PositionedBinaryLeaf) =>
          Bound(leftTree.x - 1, x)
        case (PositionedBinaryLeaf, rightTree: PositionedBinaryNode[K, V]) =>
          Bound(x, rightTree.x + 1)
        case (leftTree: PositionedBinaryNode[K, V], rightTree: PositionedBinaryNode[K, V]) =>
          Bound(leftTree.x - 1, rightTree.x + 1)
      }
    }

    val maxH = Math.max(maxOption(leftBounds.keys, 0), maxOption(rightBounds.keys, 0))
    val childrenBoundaries = (1 to maxH).map { h =>
      val lBoundOption = leftBounds.get(h)
      val rBoundOption = rightBounds.get(h)

      (h + 1) -> {
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

  override def compactTree(isLeftMost: Boolean = false): PositionedBinaryTree[K, V] = {
    val compactedLeft = left.compactTree(isLeftMost)
    val compactedRight = right.compactTree()
    val leftBounds = compactedLeft.bounds
    val rightBounds = compactedRight.bounds

    def deltaAtLevelH(h: Int): Option[Int] = {
      (leftBounds.get(h), rightBounds.get(h)) match {
        case (Some(_), None) => None
        case (None, Some(rB)) => Some(rB.left - 1)
        case (Some(lB), Some(rB)) => Some(rB.left - lB.right - 1)
        //INVARIANT: case (None, None) isn't possible by construction
        case _ => throw new RuntimeException
      }
    }
    val rootDelta = (isLeftMost, compactedLeft) match {
      case (false, _) => 0
      case (_, PositionedBinaryLeaf) => x - 1
      case (_, leftNode: PositionedBinaryNode[K, V]) => x - leftNode.x - 1
      //INVARIANT: Can't happen by construction of compactedLeft
      case _ => throw new RuntimeException
    }

    val maxH = Math.max(maxOption(leftBounds.keys, 0), maxOption(rightBounds.keys, 0))

    val maxDelta = minOption((1 to maxH)
      .map(deltaAtLevelH)
      .filter(_.isDefined).map(_.get), 0)

    if (maxDelta > 0) {
      new PositionedBinaryNode[K, V](key, compactedLeft, compactedRight.moveToLeft(maxDelta, x - rootDelta), value, x - rootDelta, y)
    } else {
      new PositionedBinaryNode[K, V](key, compactedLeft, compactedRight, value, x - rootDelta, y)
    }
  }

  override private[tree] def moveToLeft(delta: Int, parentX: Int): PositionedBinaryTree[K, V] = {
    //INVARIANT: delta > 0
    val newX = Math.max(x - delta, parentX + 1)
    new PositionedBinaryNode[K, V](key, left.moveToLeft(delta, 0), right.moveToLeft(delta, newX), value, newX, y)
  }

}

object PositionedBinaryNode {
  def apply[K, V](key: K, x: Int, y: Int) =
    new PositionedBinaryNode[K, V](key, PositionedBinaryLeaf, PositionedBinaryLeaf, None, x, y)
  def apply[K, V](key: K, value: V, x: Int, y: Int) =
    new PositionedBinaryNode[K, V](key, PositionedBinaryLeaf, PositionedBinaryLeaf, Some(value), x, y)

  private[tree] def maxOption(s: Iterable[Int], defaultValue: Int): Int = {
    s.reduceOption(Math.max(_, _)).getOrElse(defaultValue)
  }

  private[tree] def minOption(s: Iterable[Int], defaultValue: Int): Int = {
    s.reduceOption(Math.min(_, _)).getOrElse(defaultValue)
  }
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

private case class Bound(left: Int, right: Int)