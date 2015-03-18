package net.soundmining.example

import net.soundmining.MusicActor.MusicActorPattern
import net.soundmining._
import Spectrum._
import org.scalatest.FunSuite
import scala.collection.JavaConversions._
import java.{lang => jl}
import Utils._

import Melody._
import Pattern._
import MusicActor._

/**
 * Examples for "From Lisp to Scala"
 */
class MusicPlayerExample extends FunSuite {

  sealed case class AddAction(action: Integer)

  object HEAD_ACTION extends AddAction(new Integer(0))

  object TAIL_ACTION extends AddAction(new Integer(1))

  object BEFORE_ACTION extends AddAction(new Integer(2))

  object AFTER_ACTION extends AddAction(new Integer(3))

  trait AbstractInstrumentBuilder {
    val addAction: AddAction
    val nodeId: jl.Integer
    val dur: jl.Float
    val freq: jl.Float
    val pan: jl.Float
    val attack: jl.Float
    val amp: jl.Float
    val instrumentName: String

    def build(): Seq[Object] = {
      Seq(
        instrumentName,
        new Integer(-1), addAction.action, nodeId,
        "dur", dur,
        "freq", freq,
        "pan", pan,
        "attack", attack,
        "amp", amp
      )
    }
  }

  case class SineInstrument(addAction: AddAction = HEAD_ACTION, nodeId: jl.Integer = 0,
                            dur: jl.Float = 1,
                            freq: jl.Float = 440,
                            pan: jl.Float = 0,
                            attack: jl.Float = 0.01f,
                            amp: jl.Float = 1) extends AbstractInstrumentBuilder {
    override val instrumentName: String = "sine"
  }

  case class TriangleInstrument(addAction: AddAction = HEAD_ACTION, nodeId: jl.Integer = 0,
                                dur: jl.Float = 1,
                                freq: jl.Float = 440,
                                pan: jl.Float = 0,
                                attack: jl.Float = 0.01f,
                                amp: jl.Float = 1) extends AbstractInstrumentBuilder {
    override val instrumentName: String = "triangle"
  }

  case class PulseInstrument(addAction: AddAction = HEAD_ACTION, nodeId: jl.Integer = 0,
                             dur: jl.Float = 1,
                             freq: jl.Float = 440,
                             pan: jl.Float = 0,
                             attack: jl.Float = 0.01f,
                             amp: jl.Float = 1) extends AbstractInstrumentBuilder {
    override val instrumentName: String = "pulse"
  }

  test("Play some notes") {
    val player = MusicPlayer()
    player.startPlay()

    player.sendNew(0,
      SineInstrument(freq = 440f, dur = 1f, attack = 0.1f, amp = 0.5f).build())
    player.sendNew(absoluteTimeToMillis(1),
      TriangleInstrument(freq = 4660f, dur = 0.01f, attack = 0.001f).build())
    player.sendNew(absoluteTimeToMillis(2),
      TriangleInstrument(freq = 500f, dur = 3f, attack = 1.5f, amp = 0.1f).build())
    player.sendNew(absoluteTimeToMillis(3),
      PulseInstrument(freq = 4f, dur = 5f, attack = 2.5f, amp = 0.1f).build())

    Thread.sleep(1000)
  }

  val harmonicSpectrum = makeSpectrum(20, 2, 150)

  test("Play a Spectrum") {
    println(harmonicSpectrum)
    val player = MusicPlayer()
    player.startPlay()

    (0 until harmonicSpectrum.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(i * 0.1f),
        SineInstrument(
          freq = harmonicSpectrum(i),
          dur = 1f,
          attack = 0.5f,
          amp = 1 - (i * 0.01f)).build())
    }
    Thread.sleep(5000)
  }

  val nonHarmonicSpectrum = makeSpectrum(20, phi, 150)

  test("Play a non-harmonic Spectrum") {
    println(nonHarmonicSpectrum)
    val player = MusicPlayer()
    player.startPlay()

    (0 until nonHarmonicSpectrum.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(i * 0.1f),
        SineInstrument(
          freq = nonHarmonicSpectrum(i),
          dur = 1f,
          attack = 0.5f,
          amp = 1 - (i * 0.01f)).build())
    }
    Thread.sleep(5000)
  }
  val invertedSpectrum = makeInvertedSpectrum(harmonicSpectrum.last, 2, 150)

  test("Play a inverted Spectrum") {
    println(nonHarmonicSpectrum)
    val player = MusicPlayer()
    player.startPlay()

    (0 until invertedSpectrum.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(i * 0.1f),
        SineInstrument(
          freq = invertedSpectrum(i),
          dur = 1f,
          attack = 0.5f,
          amp = 1 - (i * 0.01f)).build())
    }
    Thread.sleep(5000)
  }

  val melody = Seq(20, 22, 24, 23, 26, 21, 25, 19)
  val durations = Seq(0.51f, 0.51f, 1f, 0.5f, 0.5f, 1f, 1f, 1f)
  val amp = Seq(0.8f, 0.5f, 0.6f, 0.7f, 0.5f, 0.6f, 0.8f, 0.6f)

  test("Play a melody") {
    val times = absolute(0, durations)
    val concreteMelody = concrete(melody, harmonicSpectrum)

    val player = MusicPlayer()
    player.startPlay()

    (0 until concreteMelody.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(times(i)),
        SineInstrument(
          freq = concreteMelody(i),
          dur = 1f,
          attack = 0.3f,
          amp = amp(i)).build())
    }
    Thread.sleep(5000)
  }

  test("Play a melody on different spectrum") {
    val times = absolute(0, durations)
    val concreteMelody = concrete(melody, nonHarmonicSpectrum)

    val player = MusicPlayer()
    player.startPlay()

    (0 until concreteMelody.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(times(i)),
        SineInstrument(
          freq = concreteMelody(i),
          dur = 1f,
          attack = 0.3f,
          amp = amp(i)).build())
    }
    Thread.sleep(5000)
  }

  test("Play a inverted melody") {
    val times = absolute(0, durations)
    val concreteMelody = concrete(melody, harmonicSpectrum)

    val player = MusicPlayer()
    player.startPlay()

    (0 until concreteMelody.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(times(i)),
        SineInstrument(
          freq = concreteMelody(i),
          dur = 1f,
          attack = 0.3f,
          amp = amp(i)).build())
    }

    val invertedTransposed = concrete(transpose(-10, inverse(melody)), harmonicSpectrum)
    val retrogradeTimes = absolute(6, retrograde(durations))

    (0 until invertedTransposed.size).foreach {
      i => player.sendNew(absoluteTimeToMillis(retrogradeTimes(i)),
        SineInstrument(
          freq = invertedTransposed(i),
          dur = 1f,
          attack = 0.3f,
          amp = amp(i)).build())
    }

    Thread.sleep(5000)
  }

  test("Play a melody pattern") {
    val melodyPattern =
      cycle(
        cycle(atom(20), atom(21)),
        atom(22),
        palindrome(atom(24), atom(23)))

    val timePattern =
      cycle(
        atom(1f),
        cycle(atom(1f), atom(0.5f)),
        cycle(atom(0.5f), atom(0.5f), atom(1f)))

    val durations = cycle(atom((0.25f, 0.01f)), atom((0.5f, 0.4f)))

    val amp = cycle(atom(0.7f), atom(0.3f), atom(0.5f))

    val player = MusicPlayer()
    player.startPlay()

    var time = 0f
    (0 until 15).foreach {
      i =>
        val (dur, attack) = durations.takeItem()
        player.sendNew(absoluteTimeToMillis(time),
          SineInstrument(
            freq = harmonicSpectrum(melodyPattern.takeItem()),
            dur = dur,
            attack = attack,
            amp = amp.takeItem()).build())
        time += timePattern.takeItem()
    }
    Thread.sleep(5000)
  }

  case class MakeMelodyEvent(size: Int, startTime: Float) extends MusicEvent

  case class SineMelodyMakerActor(melody: PatternType[Int],
                                  spectrum: Seq[Float],
                                  rhythm: PatternType[Float],
                                  duration: PatternType[(Float, Float)],
                                  amp: PatternType[Float],
                                  var listeners: MusicActorPattern = emptyActor) extends NodeActor {
    def receive = {
      case MakeMelodyEvent(size, startTime) =>
        var time = startTime
        (0 until size).foreach {
          i =>
            val (dur, attack) = duration.takeItem()
            val instrument =
              SineInstrument(
                freq = spectrum(melody.takeItem()),
                dur = dur,
                attack = attack,
                amp = amp.takeItem())
            listeners.takeItem().tell(PlayInstrumentEvent(time, instrument))
            time += rhythm.takeItem()
        }
    }
  }

  case class PlayInstrumentEvent(time: Float, instrument: AbstractInstrumentBuilder)
    extends MusicEvent

  case class PlayerActor(implicit player: MusicPlayer) extends LeafActor {
    def receive = {
      case PlayInstrumentEvent(time, instrument) =>
        player.sendNew(absoluteTimeToMillis(time), instrument.build())
    }
  }

  test("Play via musical actor") {
    val melody =
      cycle(
        cycle(atom(20), atom(21)),
        atom(22),
        cycle(atom(24), atom(23)))

    val rhythm =
      cycle(
        atom(1f),
        cycle(atom(1f), atom(0.5f)),
        cycle(atom(0.5f), atom(0.5f), atom(1f)))

    val durations = cycle(atom((0.25f, 0.01f)), atom((0.5f, 0.4f)))

    val amp = palindrome(atom(0.7f), atom(0.3f), atom(0.5f))

    implicit val player = MusicPlayer()
    player.startPlay()

    val playerActor = PlayerActor()

    val melodyActor =
      withActor(SineMelodyMakerActor(melody, harmonicSpectrum, rhythm, durations, amp)) {
        _.listen(playerActor)
      }

    melodyActor.tell(MakeMelodyEvent(15, 0f))

    Thread.sleep(5000)
  }
}
