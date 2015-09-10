
sealed trait BinaryTree[T]
object BinaryTree {
  def leaf(): BinaryTree[Unit] = new Leaf[Unit](())
  def leaf[T](t: T): BinaryTree[T] = new Leaf[T](t)
  def bin[T](l: BinaryTree[T], r: BinaryTree[T]): BinaryTree[T] = new Bin(l,r)


  def prettyPrint[T](t: BinaryTree[T]): Unit = {
    def printHelper(pad: String, t: BinaryTree[T]): Unit = {
      t match {
        case Leaf(elem) =>
          System.out.println(s"$pad$elem")
        case Bin(l, r) =>
          System.out.println(s"$pad+-\\")
          printHelper(pad + "| ", l)
          printHelper(pad + "  ", r)
      }
    }
    printHelper("", t)
  }
}
case class Leaf[T](elem: T) extends BinaryTree[T]
case class Bin[T](left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]
