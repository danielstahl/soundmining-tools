package net.soundmining

import scala.annotation.tailrec
import scala.util.Random

object Generative {

  case class Picker[T](items: Seq[T]) {
    def pick(size: Int)(implicit random: Random): Seq[T] =
      random.shuffle(items).take(size)
  }

  case class WeightedRandom[T](pairs: Seq[(T, Double)]) {
    private val sortedPairs: Seq[(T, Double)] = sortPairs(pairs)

    private def sortPairs(pairs: Seq[(T, Double)]): Seq[(T, Double)] = {
      val fact = 1.0 / (pairs.map {
        case (value, probability) => probability
      }.sum)

      pairs.map {
        case (value, probability) => (value, probability * fact)
      }.sortBy {
        case (value, probability) => probability
      }.reverse
    }

    @tailrec
    private def chooseValue(weightedRandom: Double, pairsToChoose: Seq[(T, Double)]): T = {
      pairsToChoose match {
        case (value, probability) :: _ if weightedRandom <= probability => value
        case (value, probability) :: xs => chooseValue(weightedRandom - probability, xs)
      }
    }

    def choose()(implicit random: Random): T =
      chooseValue(random.nextDouble(), sortedPairs)
  }

  case class MarkovChain[T](nodes: Map[T, Seq[(T, Double)]], startNode: T) {
    private var currentNode: T = startNode
    private val compiledNodes: Map[T, WeightedRandom[T]] =
      nodes.map {
        case (value, pairs) => (value, WeightedRandom(pairs))
      }

    def current: T = currentNode

    def next(implicit random: Random): T = {
      currentNode = compiledNodes(currentNode).choose()
      current
    }
  }
}
