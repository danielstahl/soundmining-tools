package net.soundmining


case class Sequencer(size: Int, startTimeHandler: Double => Double, nextTimeHandler: Int => Double, stepHandlers: Seq[(Int, Double) => Unit], spawnSequences: Map[Int, Seq[Sequencer]]) {
  def generateSequence(startTime: Double): Unit = {
    var currentTime = startTimeHandler(startTime)
    (0 until size).foreach {
      i =>
        stepHandlers.foreach(_(i, currentTime))
        spawnSequences.get(i).foreach(_.foreach(_.generateSequence(currentTime)))
        currentTime = currentTime + nextTimeHandler(i)
    }
  }
}

object Sequencer {
  class Builder(sizeArg: Int) {
    private var startTimeHandlerArg: Double => Double = start => start
    private var nextTimeHandlerArg: Int => Double = _
    private var stepHandlersArg: Seq[(Int, Double) => Unit] = Seq.empty
    private var spawnSequencesArg: Map[Int, Seq[Sequencer]] = Map.empty

    def startTimeHandler(handler: Double => Double): Builder = {
      startTimeHandlerArg = handler
      this
    }

    def nextTimeHandler(handler: Int => Double): Builder = {
      nextTimeHandlerArg = handler
      this
    }

    def stepHandler(handler: (Int, Double) => Unit): Builder = {
      stepHandlersArg = stepHandlersArg :+ handler
      this
    }

    def spawnSequencerHandler(index: Int, sequencer: Sequencer): Builder = {
      val updatedSequences = spawnSequencesArg.getOrElse(index, Seq.empty) :+ sequencer
      spawnSequencesArg = spawnSequencesArg + (index -> updatedSequences)
      this
    }

    def build(): Sequencer =
      new Sequencer(sizeArg, startTimeHandlerArg, nextTimeHandlerArg, stepHandlersArg, spawnSequencesArg)
  }

  def apply(size: Int): Builder =
    new Builder(size)

}