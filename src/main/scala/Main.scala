/**
 * Created by jsuereth on 9/9/15.
 */
object Main {
  import instances._

  /** A very generic mechanism that can "mark" all nodes in a traversable with an index number for when
    * they are visited.
    *
    * @param t  The collection
    * @tparam T The type of traversable
    * @tparam E The element type
    * @return  A new collection of elements and indicies.
    */
  def zipWithIndex[T[_] : Traversable, E](t: T[E]): T[(E, Int)] = {
    val tt = implicitly[Traversable[T]]
    // For this traversal, we construct a "Context" which will keep track fo the current index.
    type IndexState[A] = State[Int, A]
    // This returns a new `State` containing our index and the next index.
    def nextIdx: State[Int, Int] =
      State { initial =>
        (initial + 1, initial)
      }
    // This method marks the index of the
    def markIdx[E](el: E): IndexState[(E, Int)] = nextIdx.map(idx => (el, idx))
    // This returns a new `State[Int, T[(E, Int)]]` that when passed a starting index, will mark all nodes.
    val markCol =
      tt.traverse[IndexState, E, (E, Int)](t)(markIdx)
    // Actually run the program, starting with an index of 1, grab the resulting sequence (not the final index)
    markCol.run(1)._2
  }

  

  /** A generic mechanism to push all nodes in a traversable into a Seq[E]
    *
    * @param t  The traversable
    * @tparam T The type of traversable
    * @tparam E The element type
    * @return A new sequence containing all the elements.
    */
  def contents[T[_]: Traversable, E](t: T[E]): Seq[E] = {
    val tt = implicitly[Traversable[T]]
    tt.reduce(t)(el => Seq(el))
  }

  def mapValues[T[_]: Traversable, E, F](t: T[E])(f: E => F): T[F] = {
    val tt = implicitly[Traversable[T]]
    tt.traverse[Id, E, F](t)(f)
  }

  // Generic mechanism to pull the size of a collection.
  def size[T[_]: Traversable, E](t: T[E]): Int = {
    val tt = implicitly[Traversable[T]]
    tt.reduce(t)(el => 1)
  }

  // Given a "structure" and a set of values, will fill out the structure.
  def assemble[T[_]: Traversable, E](t: T[Unit], values: Seq[E]): Option[T[E]] = {
    type FillState[X] = State[Seq[E], X]
    // A mechanism which will pull the next value off the sequence from the current state.
    def takeHead: FillState[Option[E]] = State { s =>
      s match {
        case Nil => (Nil, None)
        case Seq(x, xs@_*) => (xs, Some(x))
      }
    }
    val tt = implicitly[Traversable[T]]
    // Here we're traversing through BOTH the State + Option applicatives
    type SuperApp[X] = FillState[Option[X]]
    val result = tt.traverse[SuperApp, Unit,E](t)(_ => takeHead)
    result.run(values)._2
  }


  def main(args: Array[String]): Unit = {
    // THe structure of the tree we'll show.
    val initialStructure: BinaryTree[Unit] = {
      import BinaryTree._
      bin(
        bin(
          leaf(),
          bin(leaf(), leaf())
        ),
        bin(
           bin(
              leaf(),
              bin(leaf(), leaf())
           ),
           bin(leaf(), bin(leaf(), leaf()))
        )
      )
    }


    System.err.println("--- Tree ---")
    val Some(tree) = assemble(initialStructure, "Hello Pittsburgh Code and Supply, Welcome to category theory".split("\\s+"))
    BinaryTree.prettyPrint(tree)
    System.err.println("--- Indexed Tree ---")
    val indexedTree = zipWithIndex(tree)
    BinaryTree.prettyPrint(indexedTree)

    System.err.println("--- Tree Size ---")
    System.err.println(size(tree))

    System.err.println("--- Tree Shape ---")
    BinaryTree.prettyPrint(mapValues(tree)(_ => ()))

    System.err.println("--- Contents ---")
    val list = contents(tree)
    System.err.println(list)
    System.err.println("--- Indexed Contents ---")
    val indexedList = zipWithIndex(list)
    System.err.println(indexedList)


    val assembled = assemble(mapValues(tree)(_ => ()), List("Welcome", "to", "the", "future.", "we", "have", "candy", "and", "beer"))
    System.err.println("--- Filled Tree ---")
    BinaryTree.prettyPrint(assembled.get)
  }
}
