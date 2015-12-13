package org.mlarocca.s99.tree

object BinaryTree {
  /**
   *
   * @param n
   * @param key
   * @tparam K
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

  private[s99] val NegativeValueErrorMessage = "n can't be negative"
}

sealed abstract class BinaryTree[+K, +V] {
  def inOrder(): Seq[K] = Nil
  def preOrder(): Seq[K] = Nil
}

case class BinaryNode[+K, +V](key:K, left: BinaryTree[K, V], right: BinaryTree[K, V], value: Option[V] = None) extends BinaryTree[K, V] {
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
    key +: (left.inOrder() ++ right.inOrder())
  }

  override def inOrder(): Seq[K] = {
    left.inOrder() ++ (key +: right.inOrder())
  }
}
case object Leaf extends BinaryTree[Nothing, Nothing] {
  override def toString = "."

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