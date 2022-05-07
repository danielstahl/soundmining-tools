package net.soundmining.synth

object BusAllocator {
  val control: BusAllocator = BusAllocator(0)
  val audio: BusAllocator = BusAllocator(64)
}

case class BusAllocator(startChannel: Int) {

  var allocations: Map[Seq[Int], Seq[(Double, Double)]] = Map()

  def allocateNewChannels(nrOfChannels: Int): Seq[Int] = {
    if (allocations.isEmpty) {
      Range(startChannel, startChannel + nrOfChannels)
    } else {
      val maxChannel = allocations.keys.map(_.max).max
      Range(maxChannel + 1, maxChannel + 1 + nrOfChannels)
    }
  }

  def resetAllocations(): Unit = {
    println(s"Reset ${this.getClass.getName} bus allocations")
    allocations = Map()
  }

  def overlap(start: Double, end: Double, allocStart: Double, allocEnd: Double): Boolean =
    between(start, allocStart, allocEnd) ||
      between(end, allocStart, allocEnd) ||
      (start <= allocStart && end >= allocEnd)


  def allocate(nrOfChannels: Int, start: Double, end: Double): Seq[Int] = {

    val foundAllocation = allocations.find {
      case (channels, allocs) =>
        channels.size == nrOfChannels &&
          allocs.forall {
            case (allocStart, allocEnd) =>
              !overlap(start, end, allocStart, allocEnd)
          }
    }

    val allocation: (Seq[Int], Seq[(Double, Double)]) = foundAllocation match {
      case Some((channels, allocs)) =>
        (channels, allocs :+ (start, end))
      case None =>
        (allocateNewChannels(nrOfChannels), Seq((start, end)))
    }

    allocations = allocations + allocation
    allocation._1
  }

  def between(value: Double, start: Double, end: Double): Boolean =
    value >= start && value <= end
}
