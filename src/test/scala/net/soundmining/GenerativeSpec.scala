package net.soundmining

import net.soundmining.Generative.{Picker, randomRange}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random


class GenerativeSpec extends AnyFlatSpec {
  implicit val random: Random = Random

  behavior of "Picker"

  it should "pick from a list" in {

    val values = Seq(2, 7, 4)

    val picker = Picker(values)
    for(_ <- 1 to 100) {
      assert(values.contains(picker.pick(1).head))
    }
  }

  it should "pick the correct size" in {
    val values = Seq(2, 7, 4)

    val picker = Picker(values)

    for(_ <- 1 to 100) {
      val size = random.nextInt(values.size)
      assert(picker.pick(size).size === size)
    }
  }

  behavior of "Random Range"

  it should "pick a random value within a range" in {
    val min = 3.3
    val max = 5.5
    for(_ <- 1 to 100) {
      val result = randomRange(min, max)
      assert(result >= min && result <= max)
    }
  }
}
