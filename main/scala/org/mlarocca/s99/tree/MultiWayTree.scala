package org.mlarocca.s99.tree

case class MultiWayTree[+K, +V](val key: K, val value: Option[V], children: MultiWayTree[K, V]*) {
  override def toString = s"MT($key ${children.map(_.toString).mkString(", ")})"
  private def toHashString = s"MT($key[:${key.getClass.getName}}] ${children.map(_.toString).mkString(", ")})"

  lazy val size: Int = 1 + children.map(_.size).sum
  lazy val height: Int = 1 + (0 +: children.map(_.height)).max

  override def canEqual(other: Any) = {
    other.isInstanceOf[MultiWayTree[K, V]]
  }

  override def hashCode = toHashString.hashCode

  override def equals(other: Any) = other match {
    case that: MultiWayTree[_, _] if (that canEqual this) =>
      this.hashCode == that.hashCode
    case _ => false
  }
}

object MultiWayTree {
  def apply[K](key: K) = new MultiWayTree[K, Nothing](key, None)
  def apply[K, V](key: K, value: Option[V]) = new MultiWayTree[K, V](key, value)
}