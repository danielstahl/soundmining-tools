package net.soundmining

/**
 * Spectrum
 */
object Spectrum {
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
}
