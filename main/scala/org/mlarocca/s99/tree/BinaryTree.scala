package org.mlarocca.s99.tree

object BinaryTree {
  /**
   * Generates all the complete balanced trees with n nodes.
   * A complete balanced tree is a tree where the number of nodes in the right and left subtree differs at most by 1.
   *
   * @param n Number of nodes in the tree.
   * @param key The key to be inserted in the nodes.
   * @tparam K Type of tree's keys
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException]
  def cBalanced[K](n: Int, key: K): Seq[BinaryTree[K, Nothing]] = (n, n % 2) match {
    case _ if n < 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case (0, _) => Seq(Leaf)
    case (1, _) => Seq(BinaryNode(key))
    case (_, 1) =>
      val completeSubTrees = BinaryTree.cBalanced(n / 2, key);
      for {
        tLeft <- completeSubTrees
        tRight <- completeSubTrees
      } yield new BinaryNode(key, tLeft, tRight)
    case (_, 0) =>
      val m = (n - 1) / 2
      val completeSubTreesSmaller = BinaryTree.cBalanced(m, key);
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
      hBalanced(height - 1, key).map { t1 =>
        BinaryNode(key, t1, t1) +: hBalanced(height - 2, key).map { t2 =>
          Seq(BinaryNode(key, t2, t1), BinaryNode(key, t1, t2))
        }.flatten
      }.flatten
  }

  /**
   * Return the minimum number of nodes for a hBalanced tree with given height.
   * NOTE: soooo Fibonacci...
   *
   * @param height The height for the tree.
   * @throws IllegalArgumentException
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
   * Compute the maximum height an hBalanced tree with n nodes could have.
   * Slow version to double check the inductive definition.
   *
   * @param n The given number of nodes.
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
   * Compute the maximum height an hBalanced tree with n nodes could have.
   * Slow version to double check the inductive definition.
   *
   * @param n The given number of nodes.
   * @return
   */
  private [s99] def maxHbalHeightSlow(n: Int): Int =
    Stream.from(1).takeWhile(minHbalNodes(_) <= n).last

  private[s99] val NegativeValueErrorMessage = "n can't be negative"
}

abstract class BinaryTree[+K, +V] {
  def inOrder(): Seq[K]
  def preOrder(): Seq[K]
  def preOrderMirror(): Seq[K]
  def postOrder(): Seq[K]
  def hasSymmetricStructure(): Boolean
  def isSymmetric(): Boolean
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
}

trait Leaf extends BinaryTree[Nothing, Nothing] {
  override def toString = "."

  override def inOrder() = Nil
  override def preOrder() = Nil
  override def preOrderMirror() = Nil
  override def postOrder() = Nil

  override private[tree] def toPreOrderOptionList() = Seq(None)
  override private[tree] def toPreOrderMirrorOptionList() = Seq(None)

  override def hasSymmetricStructure(): Boolean = true

  override def isSymmetric(): Boolean = true
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