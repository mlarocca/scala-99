package org.mlarocca.s99.tree

object BinaryTree {
  /**
   * Generates all the complete balanced trees with n nodes.
   * A complete balanced tree is a tree where the number of nodes in the right and left subtree differs at most by 1.
   *
   * @param n Number of nodes in the tree.
   * @param key The key to be inserted in the nodes.
   * @tparam K Type of tree's keys
   * @throws IllegalArgumentException When n is negative.
   * @return
   */
  @throws[IllegalArgumentException]
  def cBalanced[K](n: Int, key: K): Seq[BinaryTree[K, Nothing]] = (n, n % 2) match {
    case _ if n < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case (0, _) => Seq(Leaf)
    case (1, _) => Seq(BinaryNode(key))
    case (_, 1) =>
      val completeSubTrees = BinaryTree.cBalanced(n / 2, key)
      for {
        tLeft <- completeSubTrees
        tRight <- completeSubTrees
      } yield new BinaryNode(key, tLeft, tRight)
    case (_, 0) =>
      val m = (n - 1) / 2
      val completeSubTreesSmaller = BinaryTree.cBalanced(m, key)
      val completeSubTreesLarger = BinaryTree.cBalanced(m + 1, key); //m + m + 1 == n

      val leftLeaning = for {
        tLeft <- completeSubTreesLarger
        tRight <- completeSubTreesSmaller
      } yield new BinaryNode(key, tLeft, tRight)

      val rightLeaning = for {
        tLeft <- completeSubTreesSmaller
        tRight <- completeSubTreesLarger
      } yield new BinaryNode(key, tLeft, tRight)

      leftLeaning ++ rightLeaning
  }

  /**
   * Generates all and only the symmetric complete balanced trees with n nodes.
   *
   * @param n Number of nodes in the tree.
   * @param key The key to be inserted in the nodes.
   * @tparam K Type of tree's keys
   * @return
   */
  def symmetricBalancedTrees[K](n: Int, key: K): Seq[BinaryTree[K, Nothing]] = {
    cBalanced(n, key).filter(_.isSymmetric())
  }

  /**
   * Generates all the hBalanced Trees of a certain height, with the same key for every node.
   * An hBalanced tree is a binary tree where left and right branches's height differ at most by 1.
   *
   * @param height The height of the balanced tree.
   * @param key The key to be inserted in the nodes.
   * @tparam K The type of the tree's keys.
   * @throws IllegalArgumentException when height is negative.
   * @return
   */
  @throws[IllegalArgumentException]
  def hBalanced[K](height: Int, key: K): Seq[BinaryTree[K, Nothing]] = height match {
    case _ if height < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case 0 => Seq(Leaf)
    case 1 => Seq(BinaryNode(key))
    case _ =>
      hBalanced(height - 1, key).flatMap { t1 =>
        BinaryNode(key, t1, t1) +: hBalanced(height - 2, key).flatMap { t2 =>
          Seq(BinaryNode(key, t2, t1), BinaryNode(key, t1, t2))
        }
      }
  }

  /**
   * Return the minimum number of nodes for a hBalanced tree with given height.
   * NOTE: soooo Fibonacci...
   *
   * @param height The height for the tree.
   * @throws IllegalArgumentException when height is negative.
   * @return
   */
  @throws[IllegalArgumentException]
  def minHbalNodes(height: Int): Int = height match {
    case _ if height < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case 0 => 0
    case 1 => 1
    case _ => 1 + minHbalNodes(height - 1) + minHbalNodes(height - 2)
  }

  /**
   * Compute the minimum height a tree with n nodes could have.
   *
   * @param n The number of nodes in the tree.
   * @throws IllegalArgumentException When n is negative.
   * @return
   */
  @throws[IllegalArgumentException]
  def minHbalHeight(n: Int): Int = n match {
    case _ if n < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case 0 => 0
    case _ => log2(n).toInt
  }
      /**
   * Compute the maximum height an hBalanced tree with n nodes could have.
   *
   * @param n The number of nodes in the tree.
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException]
  def maxHbalHeight(n: Int): Int = n match {
    case _ if n < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case 0 => 0
    case 1 => 1
    case _ =>
      val m = n - 1
      val h1 = maxHbalHeight(m / 2)
      val k = minHbalNodes(h1)
      1 + Math.min(h1 + 1, maxHbalHeight(m - k))
  }

  /**
   *
   * @param n The number of nodes in the trees.
   * @param key The key to be inserted in the nodes.
   * @tparam K The type of the tree's keys.
   * @throws IllegalArgumentException when n is negative.
   * @return
   */
  @throws[IllegalArgumentException]
  def hBalancedWithNodes[K](n: Int, key: K): Seq[BinaryTree[K, Nothing]] = n match {
    case _ if n < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case 0 => Seq(Leaf)
    case 1 => Seq(BinaryNode(key))
    case _ =>
      (minHbalHeight(n) to maxHbalHeight(n)).flatMap {
        BinaryTree.hBalanced(_, key).filter(_.size == n)
      }
  }

  /**
   * Generate a complete (balanced and hBalanced) binary tree.
   *
   * @param n The number of nodes in the tree.
   * @param key The key to be inserted in the nodes.
   * @tparam K The type of the tree's keys.
   * @throws IllegalArgumentException when n is negative.
   * @return
   */
  @throws[IllegalArgumentException]
  def completeBinaryTree[K](n: Int, key: K): BinaryTree[K, Nothing] = {
    def fillInLevels(id: Int): BinaryTree[K, Nothing] = if (id > n) {
      Leaf
    } else {
      BinaryNode(key, fillInLevels(2 * id), fillInLevels(2 * id + 1))
    }

    n match {
      case _ if n < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
      case _ =>
        fillInLevels(1)
    }
  }


  /**
   * Compute the maximum height an hBalanced tree with n nodes could have.
   * Slow version to double check the inductive definition.
   *
   * @param n The given number of nodes.
   * @return
   */
  private [s99] def maxHbalHeightSlow(n: Int): Int =
    Stream.from(1).takeWhile(minHbalNodes(_) <= n).last

  private[s99] val NegativeValueErrorMessage = "n can't be negative"
  private[s99] val NonPositiveValueErrorMessage = "n must be positive"

  private def log2(x: Double) = Math.log10(x) / Math.log10(2)
}

abstract class BinaryTree[+K, +V] {
  def size(): Int
  def inOrder(): Seq[K]
  def preOrder(): Seq[K]
  def preOrderMirror(): Seq[K]
  def postOrder(): Seq[K]
  def hasSymmetricStructure(): Boolean
  def isSymmetric(): Boolean
  def leafNodeCount(): Int
  def leafNodeSeq(): Seq[(K, Option[V])]
  def internalNodeSeq(): Seq[(K, Option[V])]
  def nodesAtLevel(level: Int): Seq[(K, Option[V])]
  private[tree] def toPreOrderOptionList(): Seq[Option[K]]
  private[tree] def toPreOrderMirrorOptionList(): Seq[Option[K]]
}

case class BinaryNode[+K, +V](key: K, left: BinaryTree[K, V], right: BinaryTree[K, V], value: Option[V] = None) extends BinaryTree[K, V] {
  override def toString = "T(" + key.toString + " " + left.toString + " " + right.toString + ")"

  //Combination of preorder + inorder is unique for each tree (trees with the same keys set could have the same inorder or preorder)
  override def hashCode = ((preOrder() ++ inOrder()).map(_.toString) mkString ".").hashCode()

  override def canEqual(other: Any): Boolean = {
    other.isInstanceOf[BinaryNode[K, V]]
  }

  override def equals(other: Any) = other match {
    case that: BinaryNode[K, V] => (that canEqual this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def size(): Int = {
    1 + left.size() + right.size()
  }

  override def preOrder(): Seq[K] = {
    key +: (left.preOrder() ++ right.preOrder())
  }

  override def preOrderMirror(): Seq[K] = {
    key +: (right.preOrder() ++ left.preOrder())
  }

  override def inOrder(): Seq[K] = {
    left.inOrder() ++ (key +: right.inOrder())
  }

  override def postOrder(): Seq[K] = {
    left.postOrder() ++ right.postOrder() ++ Seq(key)
  }

  override private[tree] def toPreOrderOptionList(): Seq[Option[K]] = {
    Some(key) +: (left.toPreOrderOptionList() ++ right.toPreOrderOptionList())
  }

  override private[tree] def toPreOrderMirrorOptionList(): Seq[Option[K]] = {
    Some(key) +: (right.toPreOrderMirrorOptionList() ++ left.toPreOrderMirrorOptionList())
  }

  override def hasSymmetricStructure(): Boolean = {
    val optionToBoolean: Option[K] => Boolean = {
      case Some(_)  => true
      case None => false
    }

    toPreOrderOptionList().map(optionToBoolean) == toPreOrderMirrorOptionList().map(optionToBoolean)
  }

  override def isSymmetric(): Boolean = {
    toPreOrderOptionList() == toPreOrderMirrorOptionList()
  }

  private def isLeafNode(): Boolean = (left, right) match {
    case (Leaf, Leaf) => true
    case _ => false
  }

  override def leafNodeCount(): Int = if (isLeafNode) 1 else left.leafNodeCount() + right.leafNodeCount()

  override def leafNodeSeq(): Seq[(K, Option[V])] = if (isLeafNode) {
    Seq((key, value))
  } else {
    left.leafNodeSeq() ++ right.leafNodeSeq()
  }

  override def internalNodeSeq(): Seq[(K, Option[V])] = if (isLeafNode) {
    Nil
  } else {
    (key, value) +: (left.internalNodeSeq() ++ right.internalNodeSeq())
  }

  @throws[IllegalArgumentException]
  override def nodesAtLevel(level: Int): Seq[(K, Option[V])] = level match {
    case 1 =>
      Seq((key, value))
    case _ if level > 1 =>
      left.nodesAtLevel(level - 1) ++ right.nodesAtLevel(level - 1)
    case _ =>
      throw new IllegalArgumentException(BinaryTree.NonPositiveValueErrorMessage)
  }
}

trait Leaf extends BinaryTree[Nothing, Nothing] {
  override def toString = "."

  override def size(): Int = 0

  override def inOrder() = Nil
  override def preOrder() = Nil
  override def preOrderMirror() = Nil
  override def postOrder() = Nil

  override private[tree] def toPreOrderOptionList() = Seq(None)
  override private[tree] def toPreOrderMirrorOptionList() = Seq(None)

  override def hasSymmetricStructure(): Boolean = true

  override def isSymmetric(): Boolean = true

  override def leafNodeCount(): Int = 0
  override def leafNodeSeq() = Nil
  override def internalNodeSeq() = Nil
  override def nodesAtLevel(level: Int) = if (level <= 0) {
    throw new IllegalArgumentException(BinaryTree.NonPositiveValueErrorMessage)
  } else {
    Nil
  }
}

case object Leaf extends Leaf {
  //0 == "".hashCode()
  override def hashCode = 0

  override def canEqual(other: Any):Boolean = other match {
    case Leaf => true
    case _ => false
  }
}

object BinaryNode {
  def apply[K, V](key: K): BinaryNode[K, V] = new BinaryNode(key, Leaf, Leaf)
  def apply[K, V](key: K, value: Option[V]): BinaryNode[K, V] = BinaryNode(key, Leaf, Leaf, value)
}