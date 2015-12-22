package org.mlarocca.s99.tree

sealed trait BinarySearchTree[+K, +V] extends BinaryTree[K, V] {
  def insert[T >: K <% Ordered[T], W >: V](key: T, value: Option[W]): BinarySearchTree[T, W]
}

object BinarySearchTree {
  def fromSeq[K <% Ordered[K], V](input: Seq[(K, Option[V])]): BinaryTree[K, V] = {
    input.foldLeft[BinarySearchTree[K, V]](BinarySearchLeaf) {
      case (tree, (key, value)) =>
        tree.insert(key, value)
    }
  }
}

class BinarySearchNode[+K, +V](
    override val key: K,
    override val left: BinarySearchTree[K, V],
    override val right: BinarySearchTree[K, V],
    override val value: Option[V] = None)
  extends BinaryNode[K, V](key, left, right, value)
  with BinarySearchTree[K, V] {

  def insert[T >: K  <% Ordered[T], W >: V](newKey: T, newValue: Option[W]): BinarySearchTree[T, W] = {
    if (newKey <= key) {
      new BinarySearchNode(key, left.insert(newKey, newValue), right, value)
    } else {
      new BinarySearchNode(key, left, right.insert(newKey, newValue), value)
    }
  }
}

case object BinarySearchLeaf extends Leaf with BinarySearchTree[Nothing, Nothing] {
  //0 == "".hashCode()
  override def hashCode = 0

  override def canEqual(other: Any): Boolean = other match {
    case BinarySearchNode => true
    case _ => false
  }

  override def insert[T <% Ordered[T], W](newKey: T, newValue: Option[W]): BinarySearchTree[T, W] = {
    BinarySearchNode(newKey, newValue)
  }

  override def size(): Int = 0

  override def leafCount(): Int = 1
}

object BinarySearchNode {
  def apply[K, V](key: K): BinarySearchNode[K, V] = new BinarySearchNode(key, BinarySearchLeaf, BinarySearchLeaf)
  def apply[K, V](key: K, value: Option[V]): BinarySearchNode[K, V] = new BinarySearchNode(key, BinarySearchLeaf, BinarySearchLeaf, value)
}