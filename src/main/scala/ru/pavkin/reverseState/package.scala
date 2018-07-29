package ru.pavkin

import cats.{Eval, Monoid, Traverse}
import cats.data.State
import ru.pavkin.reverseState.Demo.double
import cats.syntax.semigroup._

import scala.language.higherKinds

package object reverseState {

  def cumulative[T[_], W](input: T[W])(implicit M: Monoid[W],
                                       T: Traverse[T]): T[W] =
    T.traverse(input)(v => State[W, W](x => double(x |+| v)))
      .runA(M.empty)
      .value

  def reverseCumulativeR[T[_], W](input: T[W])(implicit M: Monoid[W],
                                               T: Traverse[T]): T[W] =
    T.traverse(input)(v =>
        ReverseState[W, W](x => Eval.later(double(v |+| x))))
      .runA(M.empty)
      .value

}
