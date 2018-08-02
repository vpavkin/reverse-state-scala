package ru.pavkin

import cats.data.State
import cats.syntax.semigroup._
import cats.{Monoid, Traverse}

import scala.language.higherKinds

package object reverseState {

  def dup[T](x: T): (T, T) = (x, x)

  def cumulative[T[_], W](input: T[W])(implicit M: Monoid[W],
                                       T: Traverse[T]): T[W] =
    T.traverse(input)(b => State[W, W](s => dup(s |+| b)))
      .runA(M.empty)
      .value

  def cumulativeR[T[_], W](input: T[W])(implicit M: Monoid[W],
                                        T: Traverse[T]): T[W] =
    T.traverse(input)(b => ReverseState[W, W](s => dup(b |+| s)))
      .runA(M.empty)
      .value

}
