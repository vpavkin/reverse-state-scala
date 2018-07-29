package ru.pavkin.reverseState

import cats.{Applicative, Eval, FlatMap, Monad}
import cats.instances.either._
import cats.syntax.bifunctor._

case class ReverseState[S, A](run: S => Eval[(S, A)]) {

  def map[B](f: A => B): ReverseState[S, B] = ReverseState(s => run(s).map { case (next, a) => next -> f(a) })

  def ap[B](ff: ReverseState[S, A => B]): ReverseState[S, B] = ReverseState(s =>
    for {
      (future, x) <- run(s)
      (past, f) <- ff.run(future)
    } yield (past, f(x)))

  def flatMap[B](f: A => ReverseState[S, B]): ReverseState[S, B] = ReverseState { s =>
    def thePast: Eval[(S, A)] = theFuture.flatMap {
      case (futureState, _) => run(futureState)
    }

    lazy val theFuture: Eval[(S, B)] =
      for {
        (pastState, a) <- thePast
        (_, b) <- f(a).run(s)
      } yield (pastState, b)

    theFuture
  }

  def runA(s: S): Eval[A] = run(s).map(_._2)
}

object ReverseState {

  implicit def monad[S]: FlatMap[ReverseState[S, ?]] =
    new FlatMap[ReverseState[S, ?]] {
      def flatMap[A, B](fa: ReverseState[S, A])(
        f: A => ReverseState[S, B]): ReverseState[S, B] =
        fa.flatMap(f)
      def tailRecM[A, B](a: A)(
        f: A => ReverseState[S, Either[A, B]]): ReverseState[S, B] =
        ReverseState[S, B](s => Monad[Eval].tailRecM[(S, A), (S, B)](s, a) {
          case (s, a) => f(a).run(s).map { case (s, ab) => ab.bimap((s, _), (s, _)) }
        })
      def map[A, B](fa: ReverseState[S, A])(f: A => B): ReverseState[S, B] = fa.map(f)
    }

  implicit def app[S]: Applicative[ReverseState[S, ?]] =
    new Applicative[ReverseState[S, ?]] {
      def pure[A](x: A): ReverseState[S, A] =
        ReverseState(s => Eval.later(s -> x))
      def ap[A, B](ff: ReverseState[S, A => B])(
        fa: ReverseState[S, A]): ReverseState[S, B] =
        fa.ap(ff)
    }
}
