package net.soundmining

import net.soundmining.Spectrum._
import org.scalatest.FunSuite

/**
 * TestCode for
 */
class SpectrumSpec extends FunSuite {

  test("make spectrum") {
    assert(makeSpectrum(110, 2, 5) === Seq[Float](110.0f, 330.0f, 550.0f, 770.0f, 990.0f))
  }

  test("make inverted spectrum") {
    assert(makeInvertedSpectrum(110, 2f, 5) === Seq[Float](110.0f, 36.666668f, 22.0f, 15.714286f, 12.222222f))
  }
}