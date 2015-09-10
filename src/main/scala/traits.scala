// Defines a functor
trait Functor[A[_]] {
  def map[X, Y](fa: A[X])(x: X => Y): A[Y]
}
// Defines a complete mapping of all types into A[_]
trait Pure[A[_]] {
  def pure[X](a: X): A[X]
}

trait ApplicativeFunctor[A[_]] extends Functor[A] with Pure[A] {
  def ap[X, Y](fa: A[X])(fb: A[X => Y]): A[Y]

  // Without this method, we go insane implementing anything.
  def map2[X,Y, Z](fa: A[X], fb: A[Y])(f: (X,Y) => Z): A[Z] = {
    val nextFunc = map(fb) { b =>  (a: X) => f(a, b) }
    ap(fa)(nextFunc)
  }
}

/** Represents a more abstract way of traversing things that has greater flexibility than iterators. */
trait Traversable[A[_]] {
  /**
   * Traverses over some kind of datastructure, running the given method against each element and aggregating
   * the results into some context F, storing it back into the same structure as this one, A.
   *
   * @param col  The datastructure
   * @param f    The element transformer
   * @tparam F   The "context" we run this computation through.
   * @tparam X   The type of elements.
   * @tparam Y   The resulting type of elements.
   * @return     The final transformation, inside the context F.
   */
  def traverse[F[_] : ApplicativeFunctor, X, Y](col: A[X])(f: X => F[Y]): F[A[Y]]

  /**
   * Reduces over a traversable
   * @param c The traversable/collection
   * @param reducer A function that takes each element and converts it into something we'll be joining tegether
   * @tparam X  The type of elements
   * @tparam M  The type we'll be returning.  This must have a Monoid so we can join all the results together.
   * @return   The result of joining together each element's transformed value.
   */
  def reduce[X, M : Monoid](c: A[X])(reducer: X => M): M = {
    // Mark each element with a `Const` for the value given by the reducer function.
    def mark(el: X) = Const[M, Any](reducer(el))
    // Traverse over each element, marking it with the reducer value and
    // joining these values using the Monoid provided.
    traverse[({type l[X]=Const[M, X]})#l, X, Any](c)(mark).value
  }
}

trait Monoid[M] {
  def combine(l: M, r: M): M
  def zero: M
}