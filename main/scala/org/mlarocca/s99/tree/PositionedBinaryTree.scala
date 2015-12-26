package org.mlarocca.s99.tree

trait PositionedBinaryTree[+K, +V] extends BinaryTree[K, V]

case object PositionedBinaryLeaf extends Leaf with PositionedBinaryTree[Nothing, Nothing]

class PositionedBinaryNode[+K, +V](override val key: K, override val left: BinaryTree[K, V], override val right: BinaryTree[K, V], override val value: Option[V], x: Int, y: Int) extends BinaryNode[K, V](key, left, right, value) with PositionedBinaryTree[K, V] {
  override def toString = s"T[$x, $y]($key $left $right)"
}