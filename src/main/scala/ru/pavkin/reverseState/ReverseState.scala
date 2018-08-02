package ru.pavkin.reverseState

import cats.{Applicative, Eval}

final class ReverseState[S, A](val runF: Eval[S => Eval[(S, A)]]) {

  def map[B](f: A => B): ReverseState[S, B] =
    new ReverseState(
      runF.map(run => s => run(s).map { case (next, a) => next -> f(a) })
    )

  def ap[B](ff: ReverseState[S, A => B]): ReverseState[S, B] = new ReverseState(Eval.now(s =>
    for {
      (future, x) <- run(s)
      (past, f) <- ff.run(future)
    } yield (past, f(x))))

  def flatMap[B](f: A => ReverseState[S, B]): ReverseState[S, B] = new ReverseState(Eval.later { s =>
    lazy val theFuture: Eval[(S, B)] = Eval.defer {
      for {
        futureState <- theFuture.map(_._1)
        (pastState, a) <- run(futureState)
        (_, b) <- f(a).run(s)
      } yield (pastState, b)
    }

    theFuture
  })

  def run(s: S): Eval[(S, A)] = runF.flatMap(f => f(s))

  def runA(s: S): Eval[A] = run(s).map(_._2)
}

object ReverseState {

  def apply[S, A](fn: S => (S, A)): ReverseState[S, A] =
    new ReverseState(Eval.now(s => Eval.now(fn(s))))

  implicit def reverseStateApplicative[S]: Applicative[ReverseState[S, ?]] =
    new Applicative[ReverseState[S, ?]] {
      override def map[A, B](fa: ReverseState[S, A])(f: A => B): ReverseState[S, B] = fa.map(f)

      def pure[A](x: A): ReverseState[S, A] = ReverseState(s => s -> x)

      def ap[A, B](ff: ReverseState[S, A => B])(fa: ReverseState[S, A]): ReverseState[S, B] = fa.ap(ff)
    }
}
