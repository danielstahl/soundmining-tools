package net.soundmining

/**
 * Spectrum
 */
object Spectrum {
  /*
  http://www.michaelnorris.info/musictheory
  http://www.michaelnorris.info/musictheory/harmonic-series-calculator
  * */


  val phi: Double = (1 + Math.sqrt(5)) / 2

  val invPhi: Double = 1 / phi

  private def internalMakeSpectrum(base: Double, fact: Double, size: Int, inverted: Boolean = false): Seq[Double] =
    for {
      i <- 0 until size
      multiplier: Double = fact * ((i + 1) - 1) + 1
    } yield if(inverted) base / multiplier else base * multiplier

  def makeSpectrum(base: Double, fact: Double, size: Int): Seq[Double] =
    internalMakeSpectrum(base, fact, size)

  def makeInvertedSpectrum(base: Double, fact: Double, size: Int): Seq[Double] =
    internalMakeSpectrum(base, fact, size, inverted = true)

  def makeSpectrum2(fundamental: Double, fact: Double, size: Int): Seq[Double] = {
    (1 to size).map(i => {
      val multiplier = ((i - 1) * fact) + 1
      fundamental * multiplier
    })
  }

  def makeFact(fundamental: Double, firstPartial: Double): Double =
    (firstPartial / fundamental) - 1

  /*
  * http://www.indiana.edu/~emusic/fm/fm.htm
  * */
  def  makeFmSynthesis(carrier: Double, modulator: Double, size: Int): Seq[(Double, Double)] =
  (0 until size)
    .map(i =>
      (math.abs(carrier + (i * modulator)), math.abs(carrier - (i * modulator))))


  def ringModulate(carrier: Double, modulator: Double): (Double, Double) =
    (carrier + modulator, carrier - modulator)
}
