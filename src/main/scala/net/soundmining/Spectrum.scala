package net.soundmining

/**
 * Spectrum
 */
object Spectrum {
  /*
  http://www.michaelnorris.info/musictheory
  http://www.michaelnorris.info/musictheory/harmonic-series-calculator
  * */


  val phi: Float = (1 + Math.sqrt(5).asInstanceOf[Float]) / 2

  val invPhi: Float = 1 / phi

  private def internalMakeSpectrum(base: Float, fact: Float, size: Int, inverted: Boolean = false): Seq[Float] =
    for {
      i <- 0 until size
      multiplier: Float = fact * ((i + 1) - 1) + 1
    } yield if(inverted) base / multiplier else base * multiplier

  def makeSpectrum(base: Float, fact: Float, size: Int): Seq[Float] =
    internalMakeSpectrum(base, fact, size)

  def makeInvertedSpectrum(base: Float, fact: Float, size: Int): Seq[Float] =
    internalMakeSpectrum(base, fact, size, inverted = true)

  def makeSpectrum2(fundamental: Float, fact: Float, size: Int): Seq[Float] = {
    (1 to size).map(i => {
      val multiplier = ((i - 1) * fact) + 1
      fundamental * multiplier
    })
  }

  def makeFact(fundamental: Float, firstPartial: Float): Float =
    (firstPartial / fundamental) - 1

  /*
  * http://www.indiana.edu/~emusic/fm/fm.htm
  * */
  def  makeFmSynthesis(carrier: Float, modulator: Float, size: Int): Seq[(Float, Float)] =
  (0 until size)
    .map(i =>
      (math.abs(carrier + (i * modulator)), math.abs(carrier - (i * modulator))))
}
