package net.soundmining

import org.scalatest.flatspec.AnyFlatSpec

class SequencerSpec extends AnyFlatSpec {
  behavior of "Sequencer"

  it should "call step handler with index" in {
    var lastIndex:Int  = -1
    val stepHandler = (index: Int, _: Double) => lastIndex = index
    val sequencer = Sequencer(2).nextTimeHandler(_ => 10.0).stepHandler(stepHandler).build()
    sequencer.generateSequence(0)
    assert(lastIndex === 1)
  }

  it should "call step handler with correct time" in {
    var times: Map[Int, Double] = Map.empty
    val stepHandler = (index: Int, time: Double) => times = times + (index -> time)
    val sequencer = Sequencer(3).nextTimeHandler(_ => 10.0).stepHandler(stepHandler).build()
    sequencer.generateSequence(0)
    assert(times === Map(0 -> 0.0, 1 -> 10.0, 2 -> 20.0))
  }

  it should "call the spawned sequence" in {
    var times: Map[Int, Double] = Map.empty

    val emptyStepHandler = (_: Int, _: Double) => {}
    val tracingStepHandler = (index: Int, time: Double) => times = times + (index -> time)
    val spawnedSequence = Sequencer(2).nextTimeHandler(_ => 1.0).stepHandler(tracingStepHandler).build()
    val sequencer = Sequencer(3).nextTimeHandler(_ => 10.0).stepHandler(emptyStepHandler).spawnSequencerHandler(1, spawnedSequence).build()
    sequencer.generateSequence(0)
    assert(times === Map(0 -> 10.0, 1 -> 11.0))
  }

  behavior of "Sequencer Builder"

  it should "handle several spawn sequences" in {
    val spawnedSequencer1 = Sequencer(2).nextTimeHandler(_ => 1.0).build()
    val spawnedSequencer2 = Sequencer(3).nextTimeHandler(_ => 2.0).build()
    val sequencer = Sequencer(2).nextTimeHandler(_ => 1.0)
      .spawnSequencerHandler(0, spawnedSequencer1)
      .spawnSequencerHandler(0, spawnedSequencer2)
      .spawnSequencerHandler(1, spawnedSequencer1)
      .build()
    assert(sequencer.spawnSequences === Map(0 -> Seq(spawnedSequencer1, spawnedSequencer2), 1 -> Seq(spawnedSequencer1)))
  }
}
