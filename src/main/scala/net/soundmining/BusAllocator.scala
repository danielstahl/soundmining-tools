package net.soundmining

import java.{lang => jl}

import scala.collection.JavaConverters._

object BusAllocator {
  val control: BusAllocator = BusAllocator(jl.Integer.valueOf(0))
  val audio: BusAllocator = BusAllocator(jl.Integer.valueOf(16))
}

case class BusAllocator(startChannel: jl.Integer) {

  var allocations: Map[Seq[Int], Seq[(Float, Float)]] = Map()

  def allocateNewChannels(nrOfChannels: Int): Seq[Int] = {
    if (allocations.isEmpty) {
      Range(startChannel, startChannel + nrOfChannels)
    } else {
      val maxChannel = allocations.keys.map(_.max).max
      Range(maxChannel + 1, maxChannel + 1 + nrOfChannels)
    }.toArray
  }

  def allocate(nrOfChannels: Int, start: Float, end: Float): Seq[Int] = {

    val foundAllocation = allocations.find {
      case (channels, allocs) =>
        channels.size == nrOfChannels &&
          allocs.forall {
            case (s, e) =>
              !between(start, s, e) && !between(end, s, e) &&
                !(start <= s && end >= e)
          }
    }

    val allocation: (Seq[Int], Seq[(Float, Float)]) = foundAllocation match {
      case Some((channels, allocs)) =>
        (channels, allocs :+ (start, end))
      case None =>
        (allocateNewChannels(nrOfChannels), Seq((start, end)))
    }

    allocations = allocations + allocation
    allocation._1
  }

  def between(value: Float, start: Float, end: Float): Boolean =
    value >= start && value <= end
}