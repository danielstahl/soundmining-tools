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

  test("make fm sidebands") {
    val expected = Seq((400.0,400.0), (450.0,350.0), (500.0,300.0), (550.0,250.0), (600.0,200.0))
    assert(makeFmSynthesis(400, 50, 5) === expected)
  }

  test("make reflected fm sidebands") {
    val expected = Seq((200.0,200.0), (600.0,200.0), (1000.0,600.0), (1400.0,1000.0), (1800.0,1400.0))
    assert(makeFmSynthesis(200, 400, 5) === expected)
  }
}