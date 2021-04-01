package net.soundmining

import net.soundmining.Spectrum._
import org.scalatest.FunSuite

/**
 * TestCode for
 */
class SpectrumSpec extends FunSuite {

  test("make spectrum") {
    assert(makeSpectrum(110, 2, 5) === Seq[Double](110.0, 330.0, 550.0, 770.0, 990.0))
  }

  test("make inverted spectrum") {
    assert(makeInvertedSpectrum(110, 2f, 5) === Seq[Double](110.0, 36.666666666666664, 22.0, 15.714285714285714, 12.222222222222221))
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