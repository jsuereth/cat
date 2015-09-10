/**
 * Created by jsuereth on 9/9/15.
 */
object instances {

  implicit object intMonoid extends Monoid[Int] {
    override def zero: Int = 0
    override def combine(l: Int, r: Int): Int = l + r
  }

  type Id[A] = A
  implicit object idInstances extends ApplicativeFunctor[Id] {
    override def ap[X, Y](fa: Id[X])(fb: Id[(X) => Y]): Id[Y] = fb(fa)
    override def map[X, Y](fa: Id[X])(x: (X) => Y): Id[Y] = x(fa)
    override def pure[X](a: X): Id[X] = a
  }

  implicit object optionInstnaces extends ApplicativeFunctor[Option] {
    override def ap[X, Y](fa: Option[X])(fb: Option[(X) => Y]): Option[Y] =
      for {
        a <- fa
        f <- fb
      } yield f(a)
    override def map[X, Y](fa: Option[X])(x: (X) => Y): Option[Y] = fa map x
    override def pure[X](a: X): Option[X] = Option(a)
  }

  implicit def seqMonoid[E]: Monoid[Seq[E]] =
    new Monoid[Seq[E]] {
      override def combine(l: Seq[E], r: Seq[E]): Seq[E] = l ++ r
      override def zero: Seq[E] = Seq.empty
    }

  implicit object seqTraversable extends Traversable[Seq] {
    def traverse[F[_] : ApplicativeFunctor, X, Y](col: Seq[X])(f: X => F[Y]): F[Seq[Y]] = {
      val apF = implicitly[ApplicativeFunctor[F]]
      val zero = apF.pure(Seq.empty[Y])
      col.foldLeft(zero) { (acc, el) =>
        apF.map2(acc, f(el)) { _ :+ _}
      }
    }
  }

  implicit object binTreeTraversable extends Traversable[BinaryTree] {
    override def traverse[F[_] : ApplicativeFunctor, X, Y](col: BinaryTree[X])(f: (X) => F[Y]): F[BinaryTree[Y]] = {
      val apF = implicitly[ApplicativeFunctor[F]]
      col match {
        case Leaf(a) => apF.ap(f(a))(apF.pure(BinaryTree.leaf(_)))
        case Bin(l, r) =>
          val fl = traverse(l)(f)
          val fr = traverse(r)(f)
          apF.map2(fl, fr)(BinaryTree.bin(_,_))
      }
    }
  }


  implicit def stateApplicativeFunctor[S]: ApplicativeFunctor[({ type A[x] = State[S, x]})#A] =
    new ApplicativeFunctor[({ type A[x] = State[S, x]})#A] {
      override def ap[X, Y](fa: State[S, X])(fb: State[S, X => Y]): State[S, Y] = {
        // The lame way...
        for {
          x <- fa
          f <- fb
        } yield f(x)
      }
      override def map[X, Y](fa: State[S, X])(f: X => Y): State[S, Y] = fa.map(f)
      override def pure[X](a: X): State[S, X] = State.apply(a)
    }


  implicit def composeApplicative[T[_] : ApplicativeFunctor, U[_] : ApplicativeFunctor] =
    new ApplicativeFunctor[({type A[x] = T[U[x]]})#A] {
      private val tt = implicitly[ApplicativeFunctor[T]]
      private val tu = implicitly[ApplicativeFunctor[U]]
      override def ap[X, Y](fa: T[U[X]])(fb: T[U[(X) => Y]]): T[U[Y]] = {
        tt.map2(fa, fb) { (ux: U[X], ub: U[(X => Y)]) =>
          tu.map2(ux, ub) { (x, f) => f(x) }
        }
      }

      override def map[X, Y](fa: T[U[X]])(x: (X) => Y): T[U[Y]] =
        tt.map(fa) { ux: U[X] =>
          tu.map(ux)(x)
        }

      override def pure[X](a: X): T[U[X]] = tt.pure(tu.pure(a))

    }
}
