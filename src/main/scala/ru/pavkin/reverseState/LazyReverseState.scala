package ru.pavkin.reverseState

import cats.{Applicative, Eval}

final class LazyReverseState[S, A](
    val runF: Eval[Eval[S] => (Eval[S], Eval[A])]) {

  def map[B](f: A => B): LazyReverseState[S, B] =
    new LazyReverseState(
      runF.map(run =>
        s =>
          run(s) match {
            case (next, a) => next -> a.map(f)
      })
    )

  def ap[B](ff: LazyReverseState[S, A => B]): LazyReverseState[S, B] =
    new LazyReverseState[S, B](Eval.later { s =>
      val p = for {
        (future, evalX) <- run(s)
        (past, evalF) <- ff.run(future)
        x <- evalX
      } yield (past, evalF.map(f => f(x)))
      (p.flatMap(_._1), p.flatMap(_._2))
    })

  def flatMap[B](
      f: Eval[A] => LazyReverseState[S, B]): LazyReverseState[S, B] = {
    LazyReverseState { s =>
      lazy val theFuture: Eval[(Eval[S], Eval[B])] = Eval.defer {
        for {
          (pastState, a) <- run(theFuture.flatMap(_._1))
          (_, b) <- f(a).run(s)
        } yield (pastState, b)
      }

      theFuture.value
    }

  }

  def run(s: Eval[S]): Eval[(Eval[S], Eval[A])] = runF.map(f => f(s))

  def runA(s: Eval[S]): Eval[A] = run(s).flatMap(_._2)
}

object LazyReverseState {

  def apply[S, A](fn: Eval[S] => (Eval[S], Eval[A])): LazyReverseState[S, A] =
    new LazyReverseState(Eval.later(s => fn(s)))

  implicit def reverseStateApplicative[S]: Applicative[LazyReverseState[S, ?]] =
    new Applicative[LazyReverseState[S, ?]] {
      override def map[A, B](fa: LazyReverseState[S, A])(
          f: A => B): LazyReverseState[S, B] =
        fa.map(f)

      def pure[A](x: A): LazyReverseState[S, A] =
        LazyReverseState(s => s -> Eval.later(x))

      override def ap[A, B](ff: LazyReverseState[S, A => B])(
          fa: LazyReverseState[S, A]): LazyReverseState[S, B] =
        fa.ap(ff)
    }
}
