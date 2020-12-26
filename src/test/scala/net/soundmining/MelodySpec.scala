package net.soundmining

import org.scalatest.FlatSpec

import Melody._

/**
 * Tests for melody
 */
class MelodySpec extends FlatSpec {
  behavior of "Melody"

  it should "transpose its values" in {
    val melody = Seq(5, 4, 7)
    assert(transpose(2, melody) === Seq(7, 6, 9))
  }

  it should "transpose its values negative" in {
    val melody = Seq(5, 4, 7)
    assert(transpose(-2, melody) === Seq(3, 2, 5))
  }

  it should "shift its values at pos" in {
    val melody = Seq(1, 2, 3, 4)
    assert(shift(2, melody) === Seq(3, 4, 1, 2))
  }

  it should "retrograde its values" in {
    val melody = Seq(5, 4, 7)
    assert(retrograde(melody) === Seq(7, 4, 5))
  }

  it should "inverse its values" in {
    val melody = Seq(5, 4, 7)
    assert(inverse(melody) === Seq(5, 6, 3))
  }

  it should "make a absolute time melody" in {
    val melody = Seq(5.0, 4.0)
    assert(absolute(0, melody) === Seq(0.0, 5.0))
  }

  it should "make a absolute time with delta" in {
    val melody = Seq(5.0, 4.0)
    assert(absolute(5, melody) === Seq(5.0, 10.0))
  }
}
