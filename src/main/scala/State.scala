
trait State[S, A] {
  def run(initial: S): (S, A)

  def map[B](f: A => B): State[S, B] = State.apply { s =>
    val (rs, ra) = run(s)
    (rs, f(ra))
  }

  def flatMap[B](f: A => State[S, B]): State[S,B] =
    State.apply { s =>
      val (s1, x1) = run(s)
      // TODO - This should probably trampoline.
      f(x1).run(s1)
    }
}
object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = {
    object FullState extends State[S,A] {
      def run(initial: S): (S,A) = f(initial)
    }
    FullState
  }
  def apply[S, A](el: A): State[S, A] = {
    object Const extends State[S,A] {
      def run(initial: S): (S,A) = (initial, el)
    }
    Const
  }
}
