package net.soundmining.synth

object Utils {
  def absoluteTimeToMillis(time: Double): Long = (time * 1000).round
}
