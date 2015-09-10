/**
 * Helper that allows us to ignore result types throw a computation and just want to join together some
 * value that has a Monoid.
 *
 * e.g. useful when you to `Traverse`, but you do not want to preserve the original collection in any way.
 */
case class Const[M, A](value: M)
object Const {
  // Implementation of functor methods that JUST joins together the data in the constant without
  // actually running any new computations.
  implicit def apFunctor[M: Monoid] = new ApplicativeFunctor[({type A[x] = Const[M, x]})#A] {
    override def pure[X](a: X): Const[M, X] = Const[M,X](implicitly[Monoid[M]].zero)
    override def ap[X, Y](fa: Const[M, X])(fb: Const[M, (X) => Y]): Const[M, Y] = {
      val joined = implicitly[Monoid[M]].combine(fa.value, fb.value)
      Const[M, Y](joined)
    }
    override def map[X, Y](fa: Const[M, X])(x: (X) => Y): Const[M, Y] =
      Const[M, Y](fa.value)
  }
}
