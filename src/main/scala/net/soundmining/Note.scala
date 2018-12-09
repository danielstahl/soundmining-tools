package net.soundmining

/**
  * A note class
  * For reference: http://www.phy.mtu.edu/~suits/notefreqs.html
  */
object Note {
  val refFreq = 16.3516f
  val logTwo = Math.log(2.0).toFloat

  val notePattern = """([ABCDEFGHabcdefgh]{1})(iss|ess)?(\d){1}""".r

  val naturalNoteCents = Map(
    "c" -> 0,
    "d" -> 200,
    "e" -> 400,
    "f" -> 500,
    "g" -> 700,
    "a" -> 900,
    "h" -> 1100
  )

  val centNotes = Map(
    0 -> "c",
    100 -> "ciss",
    200 -> "d",
    300 -> "diss",
    400 -> "e",
    500 -> "f",
    600 -> "fiss",
    700 -> "g",
    800 -> "giss",
    900 -> "a",
    1000 -> "hess",
    1100 -> "h"
  )

  val modifierCents = Map(
    "iss" -> 100,
    "ess" -> -100,
    "natural" -> 0
  )

  val octaveCents = 1200

  def noteToHertz(note: Symbol) = {
    val theMatch = notePattern.findAllIn(note.toString())
    val noteName = theMatch.group(1)
    val modifier = Option(theMatch.group(2)).getOrElse("natural")
    val octave = theMatch.group(3)

    val theNaturalCents = naturalNoteCents(noteName)
    val theModifierCents = modifierCents(modifier)
    val theOctaveCents = octave.toInt * octaveCents

    val absoluteCents = theOctaveCents + theNaturalCents + theModifierCents

    centsToHertz(absoluteCents)
  }

  def centsToHertz(absoluteCent: Float): Float = {
    refFreq * Math.pow(2, absoluteCent / 1200.0f).toFloat
  }

  def hertzToNote(hertz: Float): Symbol = {
    if(hertz == 0.0f) return 'undefined
    val pitchInAbsoluteCent: Double = 1200f * Math.log(hertz / refFreq) / logTwo
    val roundedCent = Math.round(pitchInAbsoluteCent / 100f) * 100f

    val nrOfOctaves = Math.floor(roundedCent / 1200f)

    val noteCents = roundedCent - (nrOfOctaves * 1200)
    val theNote = centNotes(noteCents.toInt)

    val absoluteNote = Symbol(theNote + nrOfOctaves.toInt)

    absoluteNote
  }

  def main(args: Array[String]): Unit = {
    import Spectrum._

    val hertz = 850.2832f
    val note = hertzToNote(hertz)

    println(note)

  }
}
