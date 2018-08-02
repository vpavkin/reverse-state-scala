package ru.pavkin.reverseState

import cats.data.State
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._

object Demo extends App {

  val intList = List(2, 3, 5, 7, 11, 13)
  val stringList = intList.map(_.toString)

  // int lists
  println(intList.scanLeft(0)(_ + _).tail)
  println(cumulative(intList))

  println(intList.scanRight(0)(_ + _).init)
  println(cumulativeR(intList))

  // string lists
  println(stringList.scanLeft("")(_ + _).tail)
  println(cumulative(stringList))

  println(stringList.scanRight("")(_ + _).init)
  println(cumulativeR(stringList))

  // scans

  val sum = new Scan[Int, Int, Int](elem => State(s => dup(s + elem)), 0)
  // List(41, 39, 36, 31, 24, 13)
  sum.scanRight(intList)

  val mean = new Scan[Double, Double, (Double, Int)](
    elem =>
      State {
        case (sum, count) =>
          ((sum + elem, count + 1), (sum + elem) / (count + 1))
    },
    (0.0, 0))
  // List(6.833333333333333, 7.8, 9.0, 10.333333333333334, 12.0, 13.0)
  mean.scanRight(intList.map(_.toDouble))

}
