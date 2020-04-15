
object TestApp extends App {

  val origin = "asdasdaaaweqbbbbasdasd"

  val result = origin
    .toSeq
    .groupBy(identity)
    .values
    .toSeq
    .sortWith((l, r) => l.length >= r.length)
    .mkString

  assert(result == "aaaaaaabbbbddddssssqwe") // группы с одинаковым кол-вом символов могут быть в произвольном порядке, например “qwe” или “eqw”

  val simpleP = TreeNode(1, None, None)

  val simpleQ = TreeNode(1, None, None)

  assert(TreeNode.isSameTree(Some(simpleP), Some(simpleQ)))

  val p = TreeNode(1, Some(TreeNode(2, None, None)), None)

  val q = TreeNode(1, None, Some(TreeNode(2, None, None)))

  assert(!TreeNode.isSameTree(Some(p), Some(q)))

  assert(Checker.isValidMail)

}

case class TreeNode[X](value: X, left: Option[TreeNode[X]], right: Option[TreeNode[X]])

object TreeNode {

  def isSameTree[T](right: Option[TreeNode[T]], left: Option[TreeNode[T]]): Boolean = {
    (right, left) match {
      case (Some(r), Some(l)) if r.value == l.value => isSameTree(r.left, l.left) && isSameTree(r.right, l.right)
      case (None, None) => true
      case _ => false
    }

  }

}

object Checker {
  def isValidMail: Boolean = {
    val input = scala.io.StdIn.readLine()
    val regex = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$".r
    regex.unapplySeq(input).isDefined
  }
}

