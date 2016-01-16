package org.mlarocca.s99.tree

case class MultiWayTree[+K, +V](val key: K, val value: Option[V], children: MultiWayTree[K, V]*) {
  override def canEqual(other: Any) = {
    other.isInstanceOf[MultiWayTree[K, V]]
  }

  override def hashCode = toHashString.hashCode

  override def equals(other: Any) = other match {
    case that: MultiWayTree[_, _] if (that canEqual this) =>
      this.hashCode == that.hashCode
    case _ => false
  }
  override def toString = s"$key${children.map(_.toString).mkString("")}^"

  lazy val size: Int = 1 + children.map(_.size).sum
  lazy val height: Int = 1 + (0 +: children.map(_.height)).max
  lazy val internalPathLength: Int = paths.sum
  lazy val toLispyString: String = children match {
    case Nil =>
      key.toString
    case _ =>
      s"(${key} ${children.map(_.toLispyString) mkString(" ")})"
  }

  def preOrder(): Seq[K] = {
    key +: children.flatMap(_.preOrder())
  }

  def postOrder(): Seq[K] = {
    children.flatMap(_.postOrder()) :+ key
  }

  private def toHashString = s"MT($key[:${key.getClass.getName}}] ${children.map(_.toString).mkString(", ")})"
  private lazy val paths: Seq[Int] = 0 +: children.flatMap(_.paths.map(_ + 1))
}

object MultiWayTree {
  def apply[K](key: K) = new MultiWayTree[K, Nothing](key, None)
  def apply[K, V](key: K, value: Option[V]) = new MultiWayTree[K, V](key, value)

  /**
   * Parse a string to build a MultiWayTree[Char, Nothing].
   * The string must be a valid caret-separated sequence of Chars.
   *
   * @param str
   * @throws IllegalArgumentException If the string can't be parsed.
   * @return The MultiwayTree constructed.
   */
  @throws[IllegalArgumentException]
  implicit def fromString(str: String): MultiWayTree[Char, Nothing] = {
    lazy val unParsableStringMessage = BinaryTree.UnParsableString.format(str)

    @throws[IllegalArgumentException]
    def findChildren(s: String): (Seq[MultiWayTree[Char, Nothing]], String) = s.headOption match {
      case None =>
        throw new IllegalArgumentException(unParsableStringMessage)
      case Some('^') =>
        (Nil, s.tail)
      case Some(c) =>
        val (child, remainder) = string2MTree(s)
        val (nextChildren, remainder2) = findChildren(remainder)
        (child +: nextChildren, remainder2)
    }

    /**
     * Parse a string to extract a valid MultiWayTree.
     *
     * @param s One ore more MultiWayTrees properly encoded.
     * @throws IllegalArgumentException If any part of the string does not encode a valid tree.
     * @return A couple with
     *         - A valid MTree
     *         - The characters left (if any) after extracting the first valid Tree encoded in the string
     *           (the extraction is eager so the larger possible tree is extracted from the beginning of the string).
     */
    @throws[IllegalArgumentException]
    def string2MTree(s: String): (MultiWayTree[Char, Nothing], String) = {
      s match {
        case InvalidStringPattern() =>
          throw new IllegalArgumentException(unParsableStringMessage)
        case SingleNodePattern(c, remainder) =>
          (MultiWayTree(c.head), remainder)
        case MultipleNodesPattern(c, remainder) =>
          val (children, r) = findChildren(remainder)
          (MultiWayTree(c.head, None,  children: _*), r)
      }
    }

    val (tree, remainder) = string2MTree(str)
    if (remainder == "") {
      tree
    } else {
      throw new IllegalArgumentException(unParsableStringMessage)
    }
  }

  @throws[IllegalArgumentException]
  def fromLispyString(str: String): MultiWayTree[String, Nothing] = {
    lazy val unParsableStringMessage = BinaryTree.UnParsableString.format(str)

    /**
     * Parse a string to find the next well formed LispTree string, and break it in two parts,
     * at the end of the tree found.
     *
     * @param s
     * @param openPCount How many open parenthesis have been encountered so far?
     * @return
     */
    def findNextMTree(s: String, openPCount: Int = 0): (Seq[Char], String) = s.headOption match {
      case None if openPCount == 0 =>
        (Nil, "")
      case None =>
        throw new IllegalArgumentException(unParsableStringMessage)
      case Some(c@'(') =>
        val (left, right) = findNextMTree(s.tail, openPCount + 1)
        (c +: left, right)
      case Some(c@')') if openPCount > 1 =>
        val (left, right) = findNextMTree(s.tail, openPCount - 1)
        (c +: left, right)
      case Some(c@')') if openPCount == 1 =>
        (Seq(c), s.tail)
      case Some(' ') if openPCount == 0 =>
        (Nil, s.tail)
      case Some(c) =>
        val (left, right) = findNextMTree(s.tail, openPCount)
        (c +: left, right)
    }

    /**
     *
     * @param s A string representing a sequence of children.
     * @return The sequence of children of current node (i. e. the sequence of
     *           rooted, top level trees that can be extracted from the string `s`).
     */
    def findChildren(s: String): Seq[MultiWayTree[String, Nothing]] = {
      //We need to remove at list the first space as the children strings are broken down including spaces.
      val sTrimmed = s.replaceFirst("^\\s", "")
      sTrimmed.headOption match {
        case None =>
          Nil
        case Some(')') =>
          throw new IllegalArgumentException(unParsableStringMessage)
        case _ =>
          val (childStr, remainder) = findNextMTree(sTrimmed)
          val child = lispString2MTree(childStr mkString (""))
          val nextChildren = findChildren(remainder)
          child +: nextChildren
      }
    }

    /**
     * Parse a string and, if it's a valid lispTree string, returns the tree that can be constructed from it.
     *
     * @param s A valid representation of the tree as a MultiwayTree
     *
     * @throws IllegalArgumentException If the string can't be parsed.
     * @return
     */
    @throws[IllegalArgumentException]
    def lispString2MTree(s: String): MultiWayTree[String, Nothing] = s match {
      case LispySingleNodePattern(key) =>
        MultiWayTree(key)
      case LispyNonRecursiveNodesPattern(key, childrenStr) =>
        val children = childrenStr.split("\\s")
            .map(_.replace("\\s", ""))
            .filter(!_.isEmpty)
            .map(MultiWayTree(_))
        MultiWayTree(key, None, children: _*)
      case LispyRecursiveNodesPattern(key, childrenStr) =>
        val children = findChildren(childrenStr)
        MultiWayTree(key, None, children: _*)
      case _ =>
        throw new IllegalArgumentException(unParsableStringMessage)
    }

    lispString2MTree(str)
  }

  private lazy val InvalidStringPattern = """(?:^$)|(?:^.?$)|(?:^\^)""".r
  private lazy val SingleNodePattern = """^([^\^])\^(.*)""".r
  private lazy val MultipleNodesPattern = """^([^\^])((?:[^\^]).*)""".r

  private lazy val LispySingleNodePattern = """^([^\(\) ]+)""".r
  private lazy val LispyNonRecursiveNodesPattern = """^\(([^\(\) ]+)((?: [^\(\) ]+)*)\)""".r
  private lazy val LispyRecursiveNodesPattern = """^\(([^\(\) ]+)((?: (?:(?:[^\(\) ]+)|(?:\(.*\))))*)\)""".r
}