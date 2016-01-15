package org.mlarocca.s99.tree

case class MultiWayTree[+K, +V](val key: K, val value: Option[V], children: MultiWayTree[K, V]*) {
  override def toString = s"$key${children.map(_.toString).mkString("")}^"
  private def toHashString = s"MT($key[:${key.getClass.getName}}] ${children.map(_.toString).mkString(", ")})"

  lazy val size: Int = 1 + children.map(_.size).sum
  lazy val height: Int = 1 + (0 +: children.map(_.height)).max
  private lazy val paths: Seq[Int] = 0 +: children.flatMap(_.paths.map(_ + 1))
  lazy val internalPathLength: Int = paths.sum

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
  
  @throws[IllegalArgumentException]
  implicit def fromString(str: String): MultiWayTree[Char, Nothing] = {
    lazy val InvalidStringPattern = """(?:^$)|(?:^.?$)|(?:^\^)""".r
    lazy val SingleNodePattern = """^([^\^])\^(.*)""".r
    lazy val MultipleNodesPattern = """^([^\^])((?:[^\^]).*)""".r

    def string2MTree(s: String): (MultiWayTree[Char, Nothing], String) = {
      s match {
        case InvalidStringPattern() =>
          throw new IllegalArgumentException(BinaryTree.UnParsableString.format(str))
        case SingleNodePattern(c, remainder) =>
          (MultiWayTree(c.head), remainder)
        case MultipleNodesPattern(c, remainder) =>
          def findNextChild(s: String): (Seq[MultiWayTree[Char, Nothing]], String) = s.headOption match {
            case None =>
              throw new IllegalArgumentException(BinaryTree.UnParsableString.format(str))
            case Some('^') =>
              (Nil, s.tail)
            case Some(c) =>
              val (child, remainder) = string2MTree(s)
              val (nextChildren, remainder2) = findNextChild(remainder)
              (child +: nextChildren, remainder2)
          }

          val (children, r) = findNextChild(remainder)
          (MultiWayTree(c.head, None,  children: _*), r)
      }
    }

    val (tree, remainder) = string2MTree(str)
    if (remainder == "") {
      tree
    } else {
      throw new IllegalArgumentException(BinaryTree.UnParsableString.format(str))
    }
  }
}