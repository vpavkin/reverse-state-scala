package ru.pavkin.reverseState

import cats.Traverse
import cats.data.State
import cats.syntax.traverse._

import scala.language.higherKinds

class Scan[A, B, S](step: A => State[S, B], initial: S) {
  def scan[F[_]: Traverse](fa: F[A]): F[B] =
    fa.traverse(step).runA(initial).value

  def scanRight[F[_]: Traverse](fa: F[A]): F[B] =
    fa.traverse(a => new ReverseState(step(a).runF)).runA(initial).value
}
