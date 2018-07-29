package ru.pavkin.reverseState

import cats.Eval
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._

object Demo extends App {
  def double[T](x: T): (T, T) = (x, x)

  val intList = List(2, 4, 3, 5, 3, 7)
  val stringList = intList.map(_.toString)

  val dupl = ReverseState[String, Int](s => Eval.later((s + s, s.length)))
  val addA = ReverseState[String, Int](s => Eval.later((s + "A", 1)))

//  val program = for {
//    a <- dupl
//    _ <- addA
//    _ <- addA
//    d <- dupl
//  } yield d
//
//  println(program.run("dumb").value)

  // int lists
  println(intList.scanLeft(0)(_ + _).tail)
  println(cumulative(intList))

  println(intList.scanRight(0)(_ + _).init)
  println(reverseCumulativeR(intList))

  // string lists
  println(stringList.scanLeft("")(_ + _).tail)
  println(cumulative(stringList))

  println(stringList.scanRight("")(_ + _).init)
  println(reverseCumulativeR(stringList))
}
